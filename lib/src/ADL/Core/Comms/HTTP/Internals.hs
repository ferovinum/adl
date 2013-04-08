{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module ADL.Core.Comms.HTTP.Internals(
  Context,
  ADL.Core.Comms.HTTP.Internals.init,
  connect,
  epOpen,
  ) where

import Prelude hiding (catch)

import Network(Socket,sClose)
import Network.BSD(getHostName,HostName)
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans(liftIO)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode as JSON


import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp(runSettingsSocket,Settings,defaultSettings,settingsPort)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Conduit(($$))
import qualified Data.Conduit as DC
import qualified Data.Conduit.List as DC
import qualified Data.Conduit.Network as DC
import qualified Network.HTTP.Conduit as HC

import qualified System.Log.Logger as L
import Control.Monad.Trans.Resource

import ADL.Core.Value
import ADL.Core.Sink
import ADL.Core.Comms.Types

data Context = Context {
  c_manager :: HC.Manager
}  

init :: IO Context
init = do
  m <- HC.newManager HC.def
  return (Context m)

instance Resource Context where
  release (Context m) = HC.closeManager m

data EndPointData = EndPointData {
  ep_context :: Context,
  ep_hostname :: HostName,
  ep_port :: Int,
  ep_reader :: ThreadId,
  ep_sinks :: TVar (Map.Map T.Text (JSON.Value -> IO ()))
}  

type SinksV = TVar (Map.Map T.Text (JSON.Value -> IO ()))

httpLogger = "HTTP"

epOpen :: Context -> Either Int (Int,Int) -> IO EndPoint
epOpen ctx eport = do
  hostname <- getHostName
  (port,socket) <- bindNewSocket eport

  -- the mapping from sink ids to the sink processing functions
  sinksv <- atomically $ newTVar Map.empty

  -- a tmvar used to communicate between the web server and the action runner
  nextactionv <- atomically $ newEmptyTMVar

  -- start the two background threads
  warptid <- forkIO $ runWarp socket sinksv nextactionv
  forkIO (runner nextactionv)

  return (epCreate (newSink hostname port sinksv) (close warptid nextactionv socket))
  where
    -- Attempt to bind either the given socket, or any in the specified range
    bindNewSocket :: Either Int (Int,Int) -> IO (Int,Socket)

    bindNewSocket (Left port) = do
      socket <- DC.bindPort port DC.HostAny
      return (port,socket)

    bindNewSocket (Right (port,maxPort))
      | port > maxPort = ioError (userError "Unable to find available port in range")
      | otherwise = catch (bindNewSocket (Left port)) tryNextPort
      where
        tryNextPort :: IOError -> IO (Int,Socket) -- is this the right exception??
        tryNextPort e = bindNewSocket (Right (port+1,maxPort))
    
    runWarp :: Socket -> SinksV -> TMVar (Maybe (IO ())) -> IO ()
    runWarp socket sinksv nextactionv = runSettingsSocket defaultSettings socket waiApplication
      where
        waiApplication :: Request -> ResourceT IO Response
        waiApplication req = do
          if requestMethod req /= methodPost
            then errResponse badRequest400 "Only POST requests are supported"
            else case pathInfo req of
                [id] -> do
                  body <- requestBody req $$ (fmap LBS.fromChunks DC.consume)
                  handleMessage id body
                _ -> errResponse badRequest400 "request must have a single path component"

        handleMessage :: T.Text -> LBS.ByteString -> ResourceT IO Response
        handleMessage sid body = do
          sinks <- liftIO $ atomically $ readTVar sinksv
          case Map.lookup sid sinks of
            Nothing -> errResponse notFound404 ("No handler for sink with SID " ++ show sid)
            (Just actionf) -> case JSON.eitherDecode' body of
              (Left emsg) -> errResponse badRequest400 ("Invalid JSON " ++ emsg)
              (Right v) -> do
                liftIO $ atomically $ putTMVar nextactionv (Just (actionf v))
                return (responseLBS ok200 [] "")

    runner :: TMVar (Maybe (IO ())) -> IO ()
    runner nextactionv = loop
      where
        loop = do
          ma <- atomically $ takeTMVar nextactionv
          case ma of
            Nothing -> return ()
            (Just action) -> do
              Control.Exception.handle actionExceptionHandler action
              loop
          
    actionExceptionHandler :: SomeException -> IO ()
    actionExceptionHandler e = do
      L.errorM httpLogger ("Failed to execute action:" ++ show e)
      return ()

    errResponse status s = do
        liftIO $ L.errorM httpLogger ("Request error: " ++ s)
        return (responseLBS status [] "")

    newSink :: forall a . (ADLValue a) => HostName -> Int -> SinksV -> Maybe T.Text -> (a -> IO ()) -> IO (LocalSink a)
    newSink hostname port sinksv msid handler = do
      sid <- case msid of
        (Just sid) -> return sid
        Nothing -> fmap (T.pack . UUID.toString) UUID.nextRandom

      let at = atype (defaultv :: a)
          sink = HTTPSink hostname port sid
      atomically $ modifyTVar sinksv (Map.insert sid (action at))
      return (lsCreate sink (closef sid))
      where
        action at v = case (aFromJSON fjf v) of
          Nothing -> L.errorM "Sink.action" 
            ("Request discarded: unable to parse value of type " ++ T.unpack at)
          (Just a) -> handler a

        fjf = JSONFlags True

        closef sid = atomically $ modifyTVar sinksv (Map.delete sid)

    close warptid nextactionv socket = do
      killThread warptid
      atomically $ putTMVar nextactionv Nothing
      sClose socket

-- | Create a new connection to a remote sink
connect :: forall a . (ADLValue a) => Context -> Sink a -> IO (SinkConnection a)
connect ctx (HTTPSink{hs_hostname=host,hs_port=port,hs_sid=sid}) = do
  return (scCreate send close)
  where
    send :: (ADLValue a) => a -> IO ()
    send a = do
      let tjf = JSONFlags True
          lbs = JSON.encode (aToJSON tjf a)
          req = req0{HC.requestBody=HC.RequestBodyLBS lbs}
      resp <- runResourceT $ HC.httpLbs req (c_manager ctx)
      return ()

    req0 = HC.def {
      HC.host = BSC8.pack host,
      HC.port = port,
      HC.path = T.encodeUtf8 sid,
      HC.method = "POST"
      }

    close :: IO ()
    close = return ()
{-# LANGUAGE OverloadedStrings #-}
module ADL.Adlc.Config.Python(
    PythonCustomType(..),
    PythonDbCustomType(..),
    PythonGenerate,
    PythonPackage,
    mkPythonCustomType,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data PythonCustomType = PythonCustomType
    { pythonCustomType_pyname :: T.Text
    , pythonCustomType_helpers :: T.Text
    , pythonCustomType_generateType :: Prelude.Bool
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkPythonCustomType :: T.Text -> T.Text -> PythonCustomType
mkPythonCustomType pyname helpers = PythonCustomType pyname helpers Prelude.False

instance AdlValue PythonCustomType where
    atype _ = "adlc.config.python.PythonCustomType"
    
    jsonGen = genObject
        [ genField "pyname" pythonCustomType_pyname
        , genField "helpers" pythonCustomType_helpers
        , genField "generateType" pythonCustomType_generateType
        ]
    
    jsonParser = PythonCustomType
        <$> parseField "pyname"
        <*> parseField "helpers"
        <*> parseFieldDef "generateType" Prelude.False

data PythonDbCustomType = PythonDbCustomType
    { pythonDbCustomType_pyDbType :: T.Text
    , pythonDbCustomType_helpers :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue PythonDbCustomType where
    atype _ = "adlc.config.python.PythonDbCustomType"
    
    jsonGen = genObject
        [ genField "pyDbType" pythonDbCustomType_pyDbType
        , genField "helpers" pythonDbCustomType_helpers
        ]
    
    jsonParser = PythonDbCustomType
        <$> parseField "pyDbType"
        <*> parseField "helpers"

type PythonGenerate = Prelude.Bool

type PythonPackage = T.Text
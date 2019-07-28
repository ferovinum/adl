{-|
Module : ADL.Compiler.Backends.Rust
Description: Rust backend for ADL

This module contains that necessary functions to generate
a rust backend from an ADL file.
-}
{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Rust(
 generate,
 RustFlags(..),
 rustModule,
 ) where

import           ADL.Compiler.AST
import           ADL.Compiler.Primitive
import           ADL.Utils.FileDiff                         (dirContents)
import           ADL.Utils.Format(template,formatText)
import qualified Data.ByteString.Lazy                       as LBS
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Text                                  as T
import qualified Data.Text.Encoding                         as T

import           ADL.Compiler.EIO
import           ADL.Compiler.Processing
import           ADL.Compiler.Utils
import           ADL.Utils.IndentedCode
import           Control.Monad                              (when)
import           Control.Monad.Trans                        (liftIO)
import           Control.Monad.Trans.State.Strict
import           Data.Foldable                              (for_)
import           Data.List                                  (inits, sortOn)
import           Data.Maybe                                 (isNothing, catMaybes, fromMaybe)
import           Data.Monoid
import           Data.Traversable                           (for)
import           System.FilePath                            (joinPath,
                                                             takeDirectory,
                                                             (<.>), (</>))

import           ADL.Compiler.Backends.Rust.Internal
import           ADL.Compiler.DataFiles

-- | Run this backend on a list of ADL modules. Check each module
-- for validity, and then generate the code for it.
generate :: AdlFlags -> RustFlags -> FileWriter -> [FilePath] -> EIOT ()
generate af rf fileWriter modulePaths = catchAllExceptions  $ do
  ms <- for modulePaths $ \modulePath -> do
    m <- loadAndCheckModule af modulePath
    let m' = fullyScopedModule m
    if (generateCode (m_annotations m'))
      then do
        generateModule rf fileWriter m'
        return (Just m')
      else do
        return Nothing
  generateModFiles rf fileWriter (catMaybes ms)

-- | Generate and the rust code for a single ADL module, and
-- save the resulting code to the  apppropriate file
generateModule :: RustFlags ->
                  FileWriter ->
                  RModule ->
                  EIO T.Text ()
generateModule rf fileWriter m0 = do
  let moduleName = m_name m
      m = associateCustomTypes getCustomType moduleName m0
      cgp = CodeGenProfile {}
      mf = execState (genModule m) (emptyModuleFile (m_name m) rf cgp)
      filePath = moduleFilePath (unRustModule (rsModule rf) <> unModuleName moduleName) <.> "rs"
  liftIO $ fileWriter filePath (genModuleCode "adlc" mf)
  where
    genModule m = do
      -- Generate each declaration
      for_ (getOrderedDecls m) $ \decl ->
        when (generateCode (d_annotations decl)) $
          case d_type decl of
            (Decl_Struct struct)   -> genStruct m decl struct
            (Decl_Union union)     -> genUnion m decl union
            (Decl_Typedef typedef) -> genTypedef m decl typedef
            (Decl_Newtype ntype)   -> genNewType m decl ntype

-- | Generate the tree of mod.rs files that link together the generated code
generateModFiles :: RustFlags -> FileWriter -> [RModule] -> EIO T.Text ()
generateModFiles rf fileWriter ms = do
  -- build up the map of required mod files
  let modfiles :: M.Map [Ident] (S.Set Ident)
      modfiles = foldr addParents M.empty ms
      addParents m map = foldr addParent map (tail (inits (unModuleName (m_name m))))
      addParent m = M.insertWith (<>) (init m) (S.singleton (last m))
  -- Write them to the output tree
  for_ (M.toList modfiles) $ \(parent,children) -> do
    let filePath = moduleFilePath (unRustModule (rsModule rf) <> parent <> ["mod"]) <.> "rs"
    let content = T.intercalate "\n" ["pub mod " <> child <> ";" | child <- S.toList children]
    liftIO $ fileWriter filePath (LBS.fromStrict (T.encodeUtf8 content))


genStruct :: CModule -> CDecl -> Struct CResolvedType -> CState ()
genStruct m decl struct@Struct{s_typeParams=typeParams} = do
  fds <- mapM genFieldDetails (s_fields struct)
  let structName = capitalise (d_name decl)
  phantomFields <- mapM phantomData phantomTypeParams
  addDeclaration $ renderCommentForDeclaration decl <> render structName typeParams fds phantomFields
  where
    render :: T.Text -> [Ident] -> [FieldDetails] -> [T.Text] -> Code
    render name typeParams fields phantomFields 
      =  renderDecl
      <> cline ""
      <> cblock (template "impl$1 $2$1" [typeParamsExpr typeParams, name])
        ( renderConstructor
        )
      where
        renderDecl = cblock (template "pub struct $1$2" [name, typeParamsExpr typeParams]) renderedFields
        renderedFields
          =   (mconcat [renderCommentForField (fdField fd) <> renderFieldDeclaration fd| fd <- fields])
          <>  (mconcat [ctemplate "phantom$1: $2," [tp, pf] | (tp,pf) <- zip phantomTypeParams phantomFields])
    
        renderFieldDeclaration :: FieldDetails -> Code
        renderFieldDeclaration fd
          = ctemplate "pub $1: $2," [structFieldName fd, fdTypeExprStr fd]

        renderConstructor = cblock (template "pub fn new($1) -> $2$3" [requiredArgs, name, typeParamsExpr typeParams]) (
          cblock name (
            mconcat [ctemplate "$1: $2," [structFieldName fd, fromMaybe (structFieldName fd) (fdDefValue fd)] | fd <- fields]
            )
          )

        requiredArgs = T.intercalate ", "
          [ template "$1: $2" [structFieldName fd, fdTypeExprStr fd]
          | fd <- fields, isNothing (fdDefValue fd) ]

    phantomTypeParams = S.toList $ S.difference
      (S.fromList typeParams)
      (S.unions [typeExprTypeParams (f_type f) | f <- s_fields struct])

genUnion :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnion m decl union@Union{u_typeParams=typeParams} = do
  fds <- mapM genFieldDetails (u_fields union)
  let unionName = capitalise (d_name decl)
  addDeclaration $ renderCommentForDeclaration decl <> render unionName typeParams fds
  where
    render :: T.Text -> [Ident] -> [FieldDetails] -> Code
    render name typeParams fields =
      cblock (template "pub enum $1$2" [name, typeParamsExpr typeParams]) renderedFields
      where
        renderedFields = mconcat [renderCommentForField (fdField fd) <> renderFieldDeclaration fd | fd <- fields]
    
        renderFieldDeclaration :: FieldDetails -> Code
        renderFieldDeclaration fd
          | isVoidType (f_type (fdField fd)) = ctemplate "$1," [enumVariantName fd]
          | otherwise = ctemplate "$1($2)," [enumVariantName fd, fdTypeExprStr fd]

    -- FIXME: workout what to do with the phantom type parameters
    phantomTypeParams = S.difference
      (S.fromList typeParams)
      (S.unions [typeExprTypeParams (f_type f) | f <- u_fields union])

genTypedef :: CModule -> CDecl -> Typedef CResolvedType -> CState ()
genTypedef m decl Typedef{t_typeParams=typeParams, t_typeExpr=te} = do
  let typeName = capitalise (d_name decl)
  typeExprStr <-genTypeExpr te
  addDeclaration $ renderCommentForDeclaration decl <> render typeName typeParams typeExprStr
  where
    render :: T.Text -> [Ident] -> T.Text -> Code
    render name typeParams typeExprStr =
      -- Unclear how to have an unused type alias parameter in rust. So
      -- we'll only include the used ones
      ctemplate "pub type $1$2 = $3;" [name, typeParamsExpr usedTypeParams, typeExprStr]

    usedTypeParams = S.toList (typeExprTypeParams te)

genNewType :: CModule -> CDecl -> Newtype CResolvedType -> CState ()
genNewType m decl Newtype{n_typeParams=typeParams, n_typeExpr=te} = do
  let typeName = capitalise (d_name decl)
  typeExprStr <-genTypeExpr te
  phantomFields <- mapM phantomData phantomTypeParams
  addDeclaration $ renderCommentForDeclaration decl <> render typeName typeParams typeExprStr phantomFields
  where
    render :: T.Text -> [Ident] -> T.Text -> [T.Text] -> Code
    render name typeParams typeExprStr phantomFields =
      ctemplate "pub struct $1$2($3);"
        [name, typeParamsExpr typeParams, T.intercalate ", " (["pub " <> typeExprStr] <> phantomFields)]

    phantomTypeParams = S.toList (S.difference (S.fromList typeParams) (typeExprTypeParams te))

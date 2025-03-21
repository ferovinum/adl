{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module ADL.Compiler.Backends.Python(
  generate,
  PythonFlags(..),
  CodeGenProfile(..),
  defaultCodeGenProfile,
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key

import ADL.Compiler.AST
import ADL.Compiler.Backends.Python.Internal
import ADL.Compiler.DataFiles
import ADL.Compiler.EIO
import ADL.Compiler.Processing
import ADL.Compiler.Primitive
import ADL.Compiler.Utils
import ADL.Utils.FileDiff
import ADL.Utils.Format
import ADL.Utils.IndentedCode

import Control.Monad(when)
import Control.Monad.State
import Control.Monad.Trans(liftIO)
import Data.Foldable(for_)
import Data.Monoid
import Data.Traversable(for)
import System.FilePath(joinPath, takeDirectory, (<.>), (</>))

generate :: AdlFlags -> PythonFlags -> FileWriter -> [FilePath] -> EIOT ()
generate af pf fileWriter modulePaths = catchAllExceptions $ do
  (ms0, tms) <- loadAndCheckModules af modulePaths
  let ms = if (af_generateTransitive af) then tms else ms0
  for ms $ \m -> do
    let m' = fullyScopedModule m
    when (generateCode (m_annotations m')) (generateModule pf fileWriter m')
    return m'
  liftIO $ print "done"
  
-- Generate and the python code for a single ADL module, and
-- save the resulting code to the apppropriate file
generateModule :: PythonFlags -> FileWriter -> RModule -> EIO T.Text ()
generateModule pf fileWriter m0 = do
  let moduleName = m_name m
      m = associateCustomTypes getCustomType moduleName m0
      fullyQualifiedPath = pfPackageName pf : unModuleName (normaliseModuleName pf moduleName)
      cgp = CodeGenProfile {
        cgpHeader = "",
        cgpMaxLineLength = 10000
      }
      mf = execState (genModule m) (emptyModuleFile moduleName fullyQualifiedPath cgp)

  -- Create __init__.py files for each module level
  let moduleComponents = scanl1 (\acc x -> acc ++ "." ++ x) (map T.unpack (init fullyQualifiedPath))
  for_ moduleComponents $ \modPath -> do
    let initPath = moduleFilePath (T.splitOn "." (T.pack modPath)) </> "__init__.py"
    liftIO $ fileWriter initPath (genCode (ctemplate "# @generated from ADL module $1" [T.pack modPath]))
  
  -- Generate the main module file
  liftIO $ fileWriter (moduleFilePath fullyQualifiedPath <.> "py") (genModuleCode pf mf)

genModule :: CModule -> CState ()
genModule m = do
  for_ (getOrderedDecls m) $ \decl ->
    when (generateCode (d_annotations decl)) $
      case d_type decl of
        (Decl_Newtype ntype)   -> genNewtype m decl ntype
        (Decl_Struct struct)   -> genStruct m decl struct
        (Decl_Typedef typedef) -> genTypedef m decl typedef
        (Decl_Union union)     -> genUnion m decl union


genNewtype :: CModule -> CDecl -> Newtype CResolvedType -> CState ()
genNewtype  m decl ntype@Newtype{n_typeParams=typeParams} = do
  typeExprOutput <- genTypeExpr (n_typeExpr ntype)
  let rootModelTypeDecl = case n_typeExpr ntype of
        TypeExpr (RT_Named _) _ -> template "\"\"$1\"\"" [typeExprOutput]
        _ -> typeExprOutput
  let classTypes = rootModelTypeDecl : (if null typeParams then [] else [template "typing.Generic$1" [typeParamsExpr typeParams]])
  let typeDecl = pyblock (template "class $1($2):" [d_name decl, T.intercalate ", " classTypes]) (cline "pass")
  addDeclaration (renderCommentForDeclaration decl <> typeDecl)

genStruct :: CModule -> CDecl -> Struct CResolvedType -> CState ()
genStruct m decl struct@Struct{s_typeParams=typeParams0} = do
  fds <- mapM genFieldDetails (s_fields struct)
  let structName = capitalise (d_name decl)
  let typeParams = prefixUnusedTypeParams (isTypeParamUsedInFields (s_fields struct)) typeParams0
  mapM_ (addModuleTypeVar (unModuleName $ m_name m)) [PYTypeVar{tvName=tp} | tp <- typeParams]
  addDeclaration $ renderClass decl typeParams fds

genTypedef :: CModule -> CDecl -> Typedef CResolvedType -> CState ()
genTypedef m decl typedef@Typedef{t_typeParams=typeParams0} = do
  typeExprOutput <- genTypeExpr (t_typeExpr typedef)
  let typeParams = prefixUnusedTypeParams (isTypeParamUsedInTypeExpr (t_typeExpr typedef)) typeParams0
  let renderedTypes = if null typeParams then "" else typeParamsExpr typeParams
  -- Check for ValidRegex annotation
  let validRegexSN = ScopedName (ModuleName ["sys", "annotations"]) "ValidRegex"
      maybeValidRegex = Map.lookup validRegexSN (d_annotations decl)
      
      -- Also check for ValidRegex in the same module
      moduleValidRegexSN = ScopedName (m_name m) "ValidRegex"
      maybeModuleValidRegex = Map.lookup moduleValidRegexSN (d_annotations decl)
      
      -- Check if ValidRegex matches expected format
      isValidRegexFormat obj = case (lookupObjectText "regex" obj, lookupObjectText "description" obj) of
        (Just _, Just _) -> True
        _ -> False
  
  case (maybeValidRegex, maybeModuleValidRegex) of
    (Just (_, JSON.Object obj), _) | isValidRegexFormat obj -> do
      let Just regex = lookupObjectText "regex" obj
          Just desc = lookupObjectText "description" obj
      let annotatedType = template "typing.Annotated[$1, pydantic.StringConstraints(pattern=r$2), $3]" 
            [typeExprOutput, escapeRegexForPython regex, template "\"$1\"" [desc]]
      let typeDecl = ctemplate "$1$2: typing.TypeAlias = $3" [d_name decl, renderedTypes, annotatedType]
      addDeclaration (renderCommentForDeclaration decl <> typeDecl)
    
    (_, Just (_, JSON.Object obj)) | isValidRegexFormat obj -> do
      let Just regex = lookupObjectText "regex" obj
          Just desc = lookupObjectText "description" obj
      let annotatedType = template "typing.Annotated[$1, pydantic.StringConstraints(pattern=r$2), $3]" 
            [typeExprOutput, escapeRegexForPython regex, template "\"$1\"" [desc]]
      let typeDecl = ctemplate "$1$2: typing.TypeAlias = $3" [d_name decl, renderedTypes, annotatedType]
      addDeclaration (renderCommentForDeclaration decl <> typeDecl)
    
    _ -> addSimpleTypeAlias typeParams renderedTypes typeExprOutput
  where
    addSimpleTypeAlias typeParams renderedTypes typeExprOutput = do
      let typeDecl = ctemplate "$1$2: typing.TypeAlias = $3" [d_name decl, renderedTypes, typeExprOutput]
      addDeclaration (renderCommentForDeclaration decl <> typeDecl)
    
    -- Helper to extract text values from JSON.Object
    lookupObjectText :: T.Text -> KM.KeyMap JSON.Value -> Maybe T.Text
    lookupObjectText key obj = case KM.lookup (Key.fromText key) obj of
      Just (JSON.String s) -> Just s
      _ -> Nothing
    
    -- Ensure regex is properly escaped for Python raw strings
    escapeRegexForPython :: T.Text -> T.Text
    escapeRegexForPython regex = 
      -- Use triple quotes if the regex contains both single and double quotes
      if T.any (== '\"') regex && T.any (== '\'') regex
        then template "\"\"\"$1\"\"\"" [regex]
        -- Otherwise use a regular raw string with double quotes
        else template "\"$1\"" [regex]

genUnion :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnion  m decl union@Union{u_typeParams=parameters} = do
  genUnionWithDiscriminate m decl union

genUnionWithDiscriminate :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnionWithDiscriminate m decl union
  | isUnionEnum union = genUnionEnum m decl union
  | otherwise = genUnionClass m decl union

genUnionEnum :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnionEnum _ decl enum = do
  fds <- mapM genFieldDetails (u_fields enum)
  let enumName = capitalise (d_name decl)
      enumDecl = pyblock (template "class $1(str, enum.Enum):" [enumName]) renderedFields
      renderedFields = mconcat [renderCommentForField (fdField fd) <> (ctemplate "$1: str = \"$1\"" [unreserveWord (fdName fd)])| fd <- fds]
  addDeclaration enumDecl

genUnionClass :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnionClass m decl union = do
  fds <- mapM genFieldDetails (u_fields union)
  mapM_ (addModuleTypeVar (unModuleName $ m_name m)) [PYTypeVar{tvName=tp} | tp <- u_typeParams union]
  addDeclaration (renderUnionChoice decl union fds)

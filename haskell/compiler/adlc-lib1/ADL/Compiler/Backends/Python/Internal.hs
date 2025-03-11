{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module ADL.Compiler.Backends.Python.Internal where

import qualified Data.Aeson as JS
import qualified Data.Aeson.Text as JST
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Semigroup as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT

import ADL.Compiler.AST
import ADL.Compiler.EIO
import ADL.Compiler.Primitive
import ADL.Compiler.Processing
import ADL.Utils.Format
import ADL.Utils.IndentedCode

import Control.Monad.State
import Control.Monad.Trans(liftIO)
import System.FilePath(joinPath)

data PythonFlags = PythonFlags {
  pfCodeGenProfile :: CodeGenProfile,
  pfPackageName :: T.Text,
  pfModuleNameNormalisation :: [ModuleName]
}

type CModule = Module (Maybe CustomType) CResolvedType
type CResolvedType = ResolvedTypeT (Maybe CustomType)
type CTypeExpr = TypeExpr CResolvedType
type CDecl = Decl (Maybe CustomType) CResolvedType
type CAnnotations = Annotations CResolvedType
type CStruct = Struct CResolvedType
type CField = Field CResolvedType

type ClassName = T.Text
type ParameterNames = [Ident]

-- ... but currently we don't support custom types, but when we do,
-- they would go here (see the java backend as an example)

data CustomType = CustomType
  deriving (Show)

-- We use a state monad to accumulate details of the python file
-- corresponding to each ADL module
type CState a = State ModuleFile a

data CodeGenProfile = CodeGenProfile {
  cgpHeader :: T.Text,
  cgpMaxLineLength :: Int
}

defaultCodeGenProfile = CodeGenProfile {
  cgpHeader = "",
  cgpMaxLineLength = 1000
}

data ModuleFile = ModuleFile {
  mfModuleName :: ModuleName,

  mfFullyQualifiedPath :: [T.Text],

  -- The imports upon which this module depends
  mfImports :: M.Map Ident PYImport,

  -- The typevars upon which this module depends
  mfTypeVars :: M.Map Ident PYTypeVar,

  -- The code
  mfDeclarations :: [Code],

  -- Details to control the code generate
  mfCodeGenProfile :: CodeGenProfile
}

data PYImport = PYImport {
  iModuleAlias :: Maybe Ident,
  iModulePath :: [Ident]
} deriving (Eq, Show, Ord)

data PYTypeVar = PYTypeVar {
  tvName :: Ident
} deriving (Eq, Show, Ord)

-- data structure to capture all of the details
-- we need for a field

data FieldDetails = FieldDetails {
  fdField       :: Field CResolvedType,
  fdName        :: T.Text,
  fdTypeExprStr :: T.Text,
  fdOptional    :: Bool,
  fdDefValue    :: Maybe T.Text,
  fdConstraints :: Maybe [T.Text]
};

-- The key functions needed to plug a type into the code generator
data TypeDetails = TypeDetails {
  -- Generate the text representation of the Python type, given the representation of the type arguments.
  -- e.g. adl type Vector<Int32> would have representation `list[int]`
  tdType :: [T.Text] -> CState T.Text,

  -- Generate a Python literal value
  tdGenLiteralText :: Literal CTypeExpr -> CState T.Text
}

addDeclaration :: Code -> CState ()
addDeclaration code = modify (\mf->mf{mfDeclarations=code:mfDeclarations mf})

reservedWords :: Set.Set Ident
reservedWords = Set.fromList
 [ "False"
  , "None"
  , "True"
  , "and"
  , "as"
  , "assert"
  , "break"
  , "class"
  , "continue"
  , "def"
  , "del"
  , "elif"
  , "else"
  , "except"
  , "finally"
  , "for"
  , "form"
  , "global"
  , "if"
  , "import"
  , "in"
  , "is"
  , "lambda"
  , "nonlocal"
  , "not"
  , "or"
  , "pass"
  , "raise"
  , "return"
  , "try"
  , "while"
  , "with"
  , "yield"
 ]

unreserveWord :: Ident -> Ident
unreserveWord n | Set.member n reservedWords = T.append n "_"
                | otherwise = n

addImport :: Ident -> PYImport -> CState ()
addImport moduleIdentity pyImport = modify (\mf->mf{mfImports=M.insert moduleIdentity pyImport (mfImports mf)})

addModulesImport :: [Ident] -> T.Text -> CState ()
addModulesImport [] _ = return ()
addModulesImport modules _ = addImport importAsName pyImport
    where
      pyImport = PYImport{iModuleAlias=Just importAsName, iModulePath=modules}
      importAsName = T.intercalate "_" modules

addTypeVar :: Ident -> PYTypeVar -> CState ()
addTypeVar moduleIdentity pyTypeVar = modify (\mf->mf{mfTypeVars=M.insert moduleIdentity pyTypeVar (mfTypeVars mf)})

addModuleTypeVar :: [Ident] -> PYTypeVar -> CState ()
addModuleTypeVar [] _ = return ()
addModuleTypeVar modules typeVar = addTypeVar moduleIdentity typeVar
  where
    moduleIdentity = T.intercalate "_" modules <> "_" <> tvName typeVar

findUnionField :: T.Text -> [Field CResolvedType] -> (Int,Field CResolvedType)
findUnionField fname fs = case L.find (\(_,f) -> f_name f == fname) (zip [0,1..] fs) of
  (Just v) -> v
  Nothing -> error ("BUG: invalid literal " <> show fname <> "for union")

generateCode :: Annotations t -> Bool
generateCode annotations = case M.lookup snPythonGenerate annotations of
  Just (_, JS.Bool gen) -> gen
  _ -> True

genCode :: Code -> LBS.ByteString
genCode code = LBS.fromStrict (TE.encodeUtf8 (T.unlines (codeText Nothing code)))

genFieldDetails :: Field CResolvedType -> CState FieldDetails
genFieldDetails field = do
  let te = f_type field
  typeExprStr <- genTypeExpr te
  defValueStr <- case f_default field of
    (Just v) -> case literalForTypeExpr te v of
      Left e -> error ("BUG: invalid json literal: " ++ T.unpack e)
      Right litv -> fmap Just (genLiteralText litv)
    Nothing -> return Nothing
  constraints <- case te of
    TypeExpr (RT_Primitive P_Word8) _ -> return (Just ["ge=0", "le=255"])
    TypeExpr (RT_Primitive P_Word16) _ -> return (Just ["ge=0", "le=65535"])
    TypeExpr (RT_Primitive P_Word32) _ -> return (Just ["ge=0", "le=4294967295"])
    TypeExpr (RT_Primitive P_Word64) _ -> return (Just ["ge=0", "le=18446744073709551615"])
    _ -> return Nothing
  return (FieldDetails field (f_name field) typeExprStr False defValueStr constraints)

genImport :: PythonFlags -> ModuleName -> PYImport -> Code
genImport pf intoModule PYImport{iModuleAlias=alias, iModulePath=path} = 
  if (formatModulePath intoModule) == modulePath
    then mempty
    else ctemplate "import $1$2" [modulePath, moduleAlias]
  where
    -- If path is in normalisation list, append "_" to first element
    moduleName = ModuleName (pfPackageName pf : unModuleName (normaliseModuleName pf (ModuleName path)))
    modulePath = formatModulePath moduleName
    moduleAlias = case alias of
      Just alias -> " as " <> alias
      Nothing -> ""
    
-- | Generate an expression to construct a literal value
genLiteralText :: Literal CTypeExpr -> CState T.Text
genLiteralText lit@(Literal (TypeExpr rt _) _) = tdGenLiteralText (getTypeDetails rt) lit

genModuleCode :: PythonFlags -> ModuleFile -> LBS.ByteString
genModuleCode pf mf = genCode code
  where
    code
      =  ctemplate "# $1generated from ADL module $2" ["@", formatText (mfModuleName mf)]
      <> cline ""
      <> genStandardImports pf
      <> cline ""
      <> mconcat [genImport pf (mfModuleName mf) i | i <- M.elems (mfImports mf), iModulePath i /= unModuleName (mfModuleName mf)]
      <> cline ""
      <> mconcat [genTypeVars pf i | i <- M.elems (mfTypeVars mf)]
      <> cline ""
      <> mconcat (L.intersperse (cline "") (reverse (mfDeclarations mf)))

genStandardImports :: PythonFlags -> Code
genStandardImports pf = mconcat [ctemplate "import $1" [i] | i <- libraries]
  where
    libraries = ["enum", "pydantic", "typing"]

-- Generate the Python type given an ADL type expression
genTypeExpr :: CTypeExpr -> CState T.Text
genTypeExpr (TypeExpr rt params) = do
  rtParamsStr <- mapM genTypeExpr params
  tdType (getTypeDetails rt) rtParamsStr

genTypeVars :: PythonFlags -> PYTypeVar -> Code
genTypeVars pf PYTypeVar{tvName=name} = ctemplate "$1 = typing.TypeVar(\"$1\")" [name]

getCustomType :: ScopedName -> RDecl -> Maybe CustomType
getCustomType _ _ = Nothing

-- Get the TypeDetails record for any resolved type
getTypeDetails :: CResolvedType -> TypeDetails

-- each primitive
getTypeDetails (RT_Primitive pt) =
  case pt of
    P_String -> primTypeDetails "str" toString
    P_Double -> primTypeDetails "float" toNumber
    P_Float -> primTypeDetails "float" toNumber
    P_Int8 -> primTypeDetails "int" toNumber
    P_Int16 -> primTypeDetails "int" toNumber
    P_Int32 -> primTypeDetails "int" toNumber
    P_Int64 -> primTypeDetails "int" toNumber
    P_Word8 -> primTypeDetails "int" toNumber
    P_Word16 -> primTypeDetails "int" toNumber
    P_Word32 -> primTypeDetails "int" toNumber
    P_Word64 -> primTypeDetails "int" toNumber
    P_Bool -> primTypeDetails "bool" toBool
    P_Void -> primTypeDetails "None" (const (return "None"))
    P_ByteVector -> primTypeDetails "bytes" toByteVector
    P_Json -> primTypeDetails "dict | None" toAny
    P_Vector -> vectorTypeDetails
    P_StringMap -> stringMapTypeDetails
    P_Nullable -> nullableTypeDetails
    P_TypeToken -> typeTokenTypeDetails
  where
    primTypeDetails t convf = TypeDetails (const (return t)) convf

    toString (Literal _ (LPrimitive (JS.String s))) = return (doubleQuote s)
    toString _ = error "BUG: expected a string literal"

    toNumber (Literal _ (LPrimitive (JS.Number n))) = return (litNumber n)
    toNumber _ = error "BUG: expected a number literal"

    toBool (Literal _ (LPrimitive (JS.Bool True))) = return "True"
    toBool (Literal _ (LPrimitive (JS.Bool False))) = return "False"
    toBool _ = error "BUG: expected a boolean literal"

    toByteVector (Literal _ (LPrimitive (JS.String v))) = return (template "bytes(\"$1\")" [v])
    toByteVector _ = error "BUG: expected a string literal for ByteVector"

    toAny (Literal _ (LPrimitive jv)) = return (jsonToText jv)
    toAny _ = error "BUG: expected a json literal for JSON"

    vectorTypeDetails = TypeDetails typeExpr literalText
      where
        typeExpr [texpr] = return (template "list[$1]" [texpr])
        typeExpr _ = error "BUG: expected a single type param for Vector"
        literalText (Literal te (LVector ls)) = do
          lits <- mapM genLiteralText ls
          return (template "[$1]" [T.intercalate ", " lits])
        literalText _ = error "BUG: invalid literal for Vector"

    stringMapTypeDetails = TypeDetails typeExpr literalText
      where
        typeExpr [texpr] = return (template "dict[str, $1]" [texpr])
        typeExpr _ = error "BUG: expected a single type param for StringMap"
        literalText (Literal _ (LStringMap m)) = do
          m' <- traverse genLiteralText m
          return (template "{$1}" [T.intercalate ", " [ template "\"$1\": $2" [doubleQuote k,v] | (k,v) <- M.toList m']])
        literalText _ = error "BUG: invalid literal for StringMap"

    nullableTypeDetails = TypeDetails typeExpr literalText
      where
        typeExpr [texpr] = return (template "typing.Union[$1, None]" [texpr])
        typeExpr _ = error "BUG: expected a single type param for Nullable"
        literalText (Literal _ (LNullable Nothing)) = return "None"
        literalText (Literal _ (LNullable (Just l))) = genLiteralText l
        literalText _ = error "BUG: invalid literal for Nullable"

    typeTokenTypeDetails = TypeDetails typeExpr literalText
      where
        typeExpr [texpr] = return (template "typing.TypeAlias[$1]" [texpr])
        typeExpr _ = error "BUG: expected a single type param for TypeToken"
        literalText _ = return "NOTCALLED"

-- a type defined through a regular ADL declaration
getTypeDetails rt@(RT_Named (scopedName,Decl{d_customType=Nothing})) = TypeDetails typeExpr literalText
  where
    (ScopedName moduleName name) = scopedName
    typeExpr typeArgs = do
      currentModuleName <- fmap mfModuleName get
      let isCurrentModule = moduleName == currentModuleName
      let modules = if isCurrentModule then [] else unModuleName moduleName
      let typeExprStr = (modulePrefix modules <> name <> typeParamsExpr typeArgs)
      addModulesImport modules name
      return (if isCurrentModule then template "\"$1\"" [typeExprStr] else typeExprStr)
    literalText (Literal (TypeExpr (RT_Named (_, Decl{d_type=Decl_Struct struct})) _) (LCtor ls)) = do
      lvs <- mapM genLiteralText ls
      return (template "{$1}" [T.intercalate ", " [template "\"$1\": $2" [f_name f,v] | (f,v) <- zip (s_fields struct) lvs]])
    literalText (Literal (TypeExpr (RT_Named (_, Decl{d_type=Decl_Newtype _})) _) (LCtor [l])) = do
      genLiteralText l
    literalText (Literal te@(TypeExpr (RT_Named (_, Decl{d_type=Decl_Union union})) _) (LUnion ctor l)) = do
      lv <- genLiteralText l
      case te of
       te | refEnumeration te -> let (i,f) = findUnionField ctor (u_fields union)
                                 in return (doubleQuote (f_name f))
          | isVoidLiteral l -> return (template "\"$1\"" [ctor])
          | otherwise -> return (template "{\"$1\": $2}" [ctor, lv])
    literalText l = error ("BUG: missing RT_Named literalText definition (" <> show l <> ")")

-- a custom type
getTypeDetails rt@(RT_Named (_,Decl{d_customType=Just customType})) =
  error "BUG: custom types not implemented"

-- a type variable
getTypeDetails (RT_Param typeVar) = TypeDetails typeExpr literalText
  where
    typeExpr _ = return typeVar
    literalText _ = return (error "BUG: literal values for type variables shouldn't be needed")

isUnionEnum :: Union CResolvedType -> Bool
isUnionEnum u = all (isVoid . f_type) (u_fields u)

isVoid :: CTypeExpr -> Bool
isVoid (TypeExpr (RT_Primitive P_Void) _)  = True
isVoid _ = False

jsonToText :: JS.Value -> T.Text
jsonToText = LT.toStrict . JST.encodeToLazyText

snPythonGenerate :: ScopedName
snPythonGenerate = ScopedName (ModuleName ["adlc", "config", "python"]) "PythonGenerate"

emptyModuleFile :: ModuleName -> [T.Text] -> CodeGenProfile -> ModuleFile
emptyModuleFile mn fqPath cgp = ModuleFile mn fqPath M.empty M.empty [] cgp

formatModulePath :: ModuleName -> T.Text
formatModulePath mn = T.intercalate "." (unModuleName mn)

normaliseModuleName :: PythonFlags -> ModuleName -> ModuleName
normaliseModuleName pf mn = case path of
      (p:ps) | mn `elem` pfModuleNameNormalisation pf -> ModuleName ((p <> "_"):ps)
      _ -> mn
  where
    path = unModuleName mn

moduleFilePath :: [Ident] -> FilePath
moduleFilePath path = joinPath (map T.unpack path)

modulePrefix :: [Ident] -> T.Text
modulePrefix [] = T.pack ""
modulePrefix modules = T.intercalate "_" modules <> "."

prefixUnusedTypeParams :: (T.Text -> Bool) -> [T.Text] -> [T.Text]
prefixUnusedTypeParams isParamUsed parameters = [if isParamUsed p then p else ("_" <> p) | p <- parameters]

typeParamsExpr :: [T.Text] -> T.Text
typeParamsExpr [] = T.pack ""
typeParamsExpr parameters = "[" <> T.intercalate ", " parameters <> "]"

renderClass :: CDecl -> ParameterNames -> [FieldDetails] -> Code
renderClass decl typeParams fields =
  CAppend renderedComments (pyblock (template "class $1($2):" [className, T.intercalate ", " (["pydantic.BaseModel"] ++ genericTypes)]) renderedFields)
  where
    className = d_name decl
    genericTypes = if null typeParams then [] else [(template "typing.Generic$1" [typeParamsExpr typeParams])]
    renderedFields = if null fields then (cline "pass") else mconcat [renderCommentForField (fdField fd) <> (ctemplate "$1: $2$3" [unreserveWord (fdName fd), fdTypeExprStr fd, renderFieldValue fd])| fd <- fields]
    renderedComments = renderCommentForDeclaration decl

    renderFieldValue :: FieldDetails -> T.Text
    renderFieldValue fd = if null params then "" else template " = pydantic.Field($1)" [T.intercalate ", " params]
      where
        defValue = maybe [] (\v -> [template "default=$1" [v]]) (fdDefValue fd)
        constraints = maybe [] (\c -> c) (fdConstraints fd)
        params = defValue ++ constraints

renderComment :: T.Text -> Code
renderComment commentValue = clineN commentLinesStarred
  where
    commentLinesStarred = ["# " <> commentLine | commentLine <- commentLinesBroken]
    commentLinesBroken = L.filter (/= "") (T.splitOn "\n" commentValue)

renderCommentForDeclaration :: CDecl -> Code
renderCommentForDeclaration decl = mconcat $ map renderDeclComment $ M.elems (d_annotations decl)
  where
    renderDeclComment :: (CResolvedType, JS.Value) -> Code
    renderDeclComment (RT_Named (ScopedName{sn_name="Doc"}, _), JS.String commentText) = renderComment commentText
    renderDeclComment _ = CEmpty

renderCommentForField :: CField -> Code
renderCommentForField field = mconcat $ map renderFieldComment $ M.elems (f_annotations field)
  where
    renderFieldComment (RT_Named (ScopedName{sn_name="Doc"}, _), JS.String commentText) = renderComment commentText
    renderFieldComment _ = CEmpty

renderUnionChoice :: CDecl -> Union CResolvedType -> [FieldDetails] -> Code
renderUnionChoice decl union fds =
  CAppend renderedComments (renderedFieldTypes <> renderedClass)
  where
    classTypes = [template "pydantic.RootModel[typing.Union[$1]]" [T.intercalate " | " fieldTypes]] ++ (if null typeParams then [] else [(template "typing.Generic$1" [typeParamsExpr typeParams])])
    typeParams = [t | t <- prefixUnusedTypeParams (isTypeParamUsedInFields (u_fields union)) (u_typeParams union), t /= "None"]
    fieldTypes = [template "$1_$2$3" [d_name decl, capitalise (fdName fd), fieldGenericTypeAnnotation fd] | fd <- fds]

    renderedFieldTypes = mconcat [pyblock (template "class $1_$2(pydantic.BaseModel$3):" [d_name decl, capitalise (fdName fd), genericTyping fd]) $ (ctemplate "$1: $2" [unreserveWord (fdName fd), fdTypeExprStr fd] <> cline "") | fd <- fds]
    renderedClass = pyblock (template "class $1($2):" [d_name decl, T.intercalate ", " classTypes]) (cline "pass")
    renderedComments = renderCommentForDeclaration decl

    fieldGenericTypeAnnotation :: FieldDetails -> T.Text
    fieldGenericTypeAnnotation fd = case f_type (fdField fd) of
      TypeExpr ( RT_Param _) _ -> (template "[$1]" [fdTypeExprStr fd])
      _ -> ""

    genericTyping :: FieldDetails -> T.Text
    genericTyping fd = case f_type (fdField fd) of
      TypeExpr ( RT_Param _) _ -> template ", typing.Generic[$1]" [fdTypeExprStr fd]
      _ -> ""

{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.PythonTables.PythonUtils where

import qualified Data.Aeson as JS
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Map as M
import qualified ADL.Compiler.AST as AST
import qualified ADL.Compiler.Backends.PythonTables.SchemaUtils as SC
import qualified ADL.Compiler.Backends.PythonTables.Schema as SC
import qualified ADL.Compiler.Backends.Python.Internal as P
import qualified ADL.Compiler.Backends.Python as P
import Control.Monad(mplus)
import Data.Maybe(fromMaybe)

import ADL.Compiler.Processing

data PythonTableFlags = PythonTableFlags {
  pt_rtpackage :: T.Text,
  pt_package :: T.Text,
  pt_crudfns :: Bool,
  pt_genversion :: GenVersion
}

data GenVersion = V1 | V2;

defaultPythonTableFlags = PythonTableFlags "adl.runtime" "adl" False V1

type DBTable = (P.CDecl,AST.Struct P.CResolvedType,SC.Table,JS.Value)

data DbColumn a
  = IdColumn SC.Column
  | DbColumn SC.Column (AST.Field P.CResolvedType) a

mkDbColumns :: Bool -> [SC.Column] -> [AST.Field P.CResolvedType] -> [DbColumn ()]
mkDbColumns False columns fields = zipWith (\c f -> DbColumn c f ()) columns fields
mkDbColumns True columns fields = (IdColumn (head columns)): mkDbColumns False(tail columns) fields

pythonFieldName :: DbColumn a -> T.Text
pythonFieldName (IdColumn _) = "id"
pythonFieldName (DbColumn _ field _) = P.unreserveWord (AST.f_name field)

withCommas :: [a] -> [(a,T.Text)]
withCommas [] = []
withCommas [l] = [(l," ")]
withCommas (l:ls) = (l,","):withCommas ls

dbTableName :: P.CDecl -> T.Text
dbTableName decl = fromMaybe (dbName (AST.d_name decl)) (tname decl `mplus` vname decl)
  where
  tname decl = annStringField dbTableType "tableName" decl
  vname decl = annStringField dbViewType "viewName" decl
  annStringField annType field decl = case getAnnotation (AST.d_annotations decl) annType of
    (Just (JS.Object hm)) -> case KM.lookup (K.fromText field) hm of
      (Just (JS.String t)) -> Just t
      _ -> Nothing
    _ -> Nothing

getAnnotation :: AST.Annotations P.CResolvedType -> AST.ScopedName -> Maybe JS.Value
getAnnotation annotations annotationName = snd <$> M.lookup annotationName annotations

dbName :: T.Text -> T.Text
dbName =  SC.toSnakeCase

getAnnotationField :: JS.Value -> T.Text -> Maybe JS.Value
getAnnotationField (JS.Object hm) field = KM.lookup (K.fromText field) hm
getAnnotationField _ _ = Nothing

dbTableType = AST.ScopedName (AST.ModuleName ["common","db"]) "DbTable"
dbViewType = AST.ScopedName (AST.ModuleName ["common","db"]) "DbView"
pythonDbTableVersion = AST.ScopedName (AST.ModuleName ["common","db"]) "PythonDbTableVersion"
pythonDbCustomType = AST.ScopedName (AST.ModuleName ["common","db"]) "PythonDbCustomType"
withDbIdType = AST.ScopedName (AST.ModuleName ["common","db"]) "WithDbId"
dbKeyType = AST.ScopedName (AST.ModuleName ["common","db"]) "DbKey"

customDbType :: SC.Column -> AST.Field P.CResolvedType -> Maybe T.Text
customDbType col field = do
    jv <- customDbAnnotation col field
    case getAnnotationField jv "pyDbType" of
      Nothing -> Nothing
      (Just (JS.String t)) -> Just t

customDbHelpers :: SC.Column -> AST.Field P.CResolvedType -> Maybe T.Text
customDbHelpers col field = do
    jv <- customDbAnnotation col field
    case getAnnotationField jv "helpers" of
      Nothing -> Nothing
      (Just (JS.String t)) -> Just t

-- The annotation can be be on the field, or on the declaration referenced by the field type
customDbAnnotation :: SC.Column -> AST.Field P.CResolvedType -> Maybe JS.Value
customDbAnnotation col field = case getAnnotation (AST.f_annotations field) pythonDbCustomType of
   (Just jv) -> Just jv
   Nothing -> case (SC.column_nullable col,AST.f_type field) of
      (False, AST.TypeExpr (RT_Named (_,decl))  _) -> getAnnotation (AST.d_annotations decl) pythonDbCustomType
      (True, AST.TypeExpr _ [AST.TypeExpr (RT_Named (_,decl)) _]) -> getAnnotation (AST.d_annotations decl) pythonDbCustomType
      _ -> Nothing

pythonTableClassName :: P.CDecl -> T.Text
pythonTableClassName decl = AST.d_name decl <> "Table"

pythonClassName :: P.CDecl -> T.Text
pythonClassName decl = AST.d_name decl

tableClassName :: P.CDecl -> T.Text
tableClassName decl = P.unreserveWord (AST.d_name decl) <> "Table" 
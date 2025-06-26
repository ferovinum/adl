{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.PythonTables.SchemaUtils where

import qualified Data.Text as T
import qualified Data.Char as Char
import qualified ADL.Compiler.Backends.PythonTables.Schema as S

toSnakeCase :: T.Text -> T.Text
toSnakeCase t = T.pack $ convert (T.unpack t)
  where
    convert (c:cs)
      | Char.isUpper c = '_' : Char.toLower c : convert cs
      | otherwise = c : convert cs
    convert [] = [] 
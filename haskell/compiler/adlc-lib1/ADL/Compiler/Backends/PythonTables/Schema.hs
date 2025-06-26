{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.PythonTables.Schema where

import qualified Data.Aeson as JS
import qualified Data.Text as T
import qualified Data.Map as M

data Table = Table
    { table_name :: T.Text
    , table_columns :: [Column]
    }
    deriving (Show)

data Column = Column
    { column_name :: T.Text
    , column_type :: T.Text
    , column_nullable :: Bool
    }
    deriving (Show) 
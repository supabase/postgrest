{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module PostgREST.SchemaCache.Table
  ( Column(..)
  , Table(..)
  , tableColumnsList
  , TablesMap
  , ColumnMap
  ) where

import qualified Data.Aeson                 as JSON
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashMap.Strict.InsOrd as HMI

import PostgREST.SchemaCache.Identifiers (FieldName,
                                          QualifiedIdentifier (..),
                                          Schema, TableName)

import Protolude


data Table = Table
  { tableSchema      :: Schema
  , tableName        :: TableName
  , tableDescription :: Maybe Text
     -- TODO Find a better way to separate tables and views
   , tableIsView     :: Bool
    -- The following fields identify what HTTP verbs can be executed on the table/view, they're not related to the privileges granted to it
  , tableInsertable  :: Bool
  , tableUpdatable   :: Bool
  , tableDeletable   :: Bool
  , tablePKCols      :: [FieldName]
  , tableColumns     :: ColumnMap
    -- Relation kind: "table" | "view" | "materialized_view" | "foreign_table".
    -- Surfaced for the typegen OpenAPI metadata extension; `tableIsView` stays
    -- for the existing view/table branching.
  , tableKind        :: Text
  }
  deriving (Show, Generic, JSON.ToJSON)

tableColumnsList :: Table -> [Column]
tableColumnsList = HMI.elems . tableColumns

instance Eq Table where
  Table{tableSchema=s1,tableName=n1} == Table{tableSchema=s2,tableName=n2} = s1 == s2 && n1 == n2

data Column = Column
  { colName               :: FieldName
  , colDescription        :: Maybe Text
  , colNullable           :: Bool
  , colType               :: Text
  , colNominalType        :: Text
  , colMaxLen             :: Maybe Int32
  , colDefault            :: Maybe Text
  , colEnum               :: [Text]
    -- Identity / generated metadata for the typegen OpenAPI extension.
    -- `colIdentityGeneration` is "ALWAYS" | "BY DEFAULT" | Nothing.
  , colIdentityGeneration :: Maybe Text
  , colIsGenerated        :: Bool
    -- pg_catalog short type name (int8, _text, user_status); typegen extension only.
  , colFormat             :: Text
  }
  deriving (Eq, Show, Ord, Generic, JSON.ToJSON)

type TablesMap = HM.HashMap QualifiedIdentifier Table
type ColumnMap = HMI.InsOrdHashMap FieldName Column

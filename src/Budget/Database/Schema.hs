{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Budget.Database.Schema where

import           Budget.Database.TH (str)

import           Data.Time (Day, LocalTime)
import           Database.HDBC (runRaw)
import           Database.HDBC.Query.TH (defineTableFromDB)
import           Database.HDBC.Schema.Driver (typeMap)
import           Database.HDBC.Schema.SQLite3 (driverSQLite3)
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           Language.Haskell.TH (Q, Dec, TypeQ)

schema :: String
schema = [str|

CREATE TABLE item_type (
  id INTEGER NOT NULL PRIMARY KEY,
  name VARCHAR NOT NULL
);

INSERT INTO item_type VALUES (1, 'income');
INSERT INTO item_type VALUES (2, 'expense');

CREATE TABLE item (
  id INTEGER NOT NULL PRIMARY KEY,
  amount INTEGER NOT NULL,
  item_type INTEGER NOT NULL,
  date DATE NOT NULL,
  name VARCHAR,
  note VARCHAR,
  crate_on DATE NOT NULL,
  FOREIGN KEY(item_type) REFERENCES item_type(id)
);

CREATE TABLE geometry (
  id INTEGER NOT NULL PRIMARY KEY,
  lat REAL NOT NULL,
  lng REAL NOT NULL
);

CREATE TABLE location (
  id INTEGER NOT NULL PRIMARY KEY,
  name VARCHAR NOT NULL,
  geometry INTEGER,
  FOREIGN KEY(geometry) REFERENCES geometry(id)
);

CREATE TABLE income_category (
  id INTEGER NOT NULL PRIMARY KEY,
  name VARCHAR NOT NULL
);

CREATE TABLE expense_category (
  id INTEGER NOT NULL PRIMARY KEY,
  name VARCHAR NOT NULL
);

CREATE TABLE expense (
  item_id INTEGER NOT NULL PRIMARY KEY,
  category INTEGER NOT NULL,
  location INTEGER,
  FOREIGN KEY(item_id) REFERENCES item(id),
  FOREIGN KEY(category) REFERENCES expense_category(id),
  FOREIGN KEY(location) REFERENCES location(id)
);

CREATE TABLE income (
  item_id INTEGER NOT NULL PRIMARY KEY,
  category INTEGER NOT NULL,
  FOREIGN KEY(item_id) REFERENCES item(id),
  FOREIGN KEY(category) REFERENCES income_category(id)
);

CREATE TABLE income_template (
  name VARCHAR NOT NULL PRIMARY KEY
);

CREATE TABLE expense_template (
  name VARCHAR NOT NULL PRIMARY KEY
);

|]

convTypes :: [(String, TypeQ)]
convTypes =
        [ ("float", [t|Double|])
        , ("date", [t|Day|])
        , ("datetime", [t|LocalTime|])
        , ("timestamp", [t|LocalTime|])
        , ("double", [t|Double|])
        , ("varchar", [t|String|])
        , ("integer", [t|Integer|])
        ]

defineTable' :: String -> String -> Q [Dec]
defineTable' source tableName =
  defineTableFromDB
    connWithSchema
    (driverSQLite3 { typeMap = convTypes })
    "main"
    tableName
    [''Show]
  where
    conn = connectSqlite3 ":memory:"
    connWithSchema = conn >>= (\c -> runRaw c source >> (return c))

defineTable :: String -> Q [Dec]
defineTable = defineTable' schema

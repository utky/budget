{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Budget.Database.Schema where

import           Budget.Database.TH (str)

import           Data.Time (Day, LocalTime, UTCTime)
import           Database.HDBC (runRaw)
import           Database.HDBC.Query.TH (defineTableFromDB)
import           Database.HDBC.Schema.Driver (typeMap)
import           Database.HDBC.Schema.SQLite3 (driverSQLite3)
import           Database.HDBC.Sqlite3 (connectSqlite3)
import           Language.Haskell.TH (Q, Dec, TypeQ)

-- FIXME: income は revenue という単語でもよさそう

schema :: String
schema = [str|

CREATE TABLE cost_type (
  id INTEGER NOT NULL PRIMARY KEY,
  name VARCHAR NOT NULL
);

INSERT INTO cost_type VALUES (1, 'fixed');
INSERT INTO cost_type VALUES (2, 'variable');

CREATE TABLE account_type (
  id INTEGER NOT NULL PRIMARY KEY,
  name VARCHAR NOT NULL
);

INSERT INTO account_type VALUES (1, 'asset');
INSERT INTO account_type VALUES (2, 'liability');

CREATE TABLE account (
  id INTEGER NOT NULL PRIMARY KEY,
  name VARCHAR NOT NULL,
  account_type INTEGER NOT NULL,
  FOREIGN KEY(account_type) REFERENCES account_type(id)
);

CREATE TABLE item_type (
  id INTEGER NOT NULL PRIMARY KEY,
  name VARCHAR NOT NULL
);

INSERT INTO item_type VALUES (1, 'income');
INSERT INTO item_type VALUES (2, 'expense');

CREATE TABLE item_category (
  id INTEGER NOT NULL PRIMARY KEY,
  name VARCHAR NOT NULL,
  item_type INTEGER NOT NULL,
  FOREIGN KEY(item_type) REFERENCES item_type(id)
);

CREATE TABLE item (
  id VARCHAR NOT NULL PRIMARY KEY,
  item_category INTEGER NOT NULL,
  date DATE NOT NULL,
  name VARCHAR NOT NULL,
  amount INTEGER NOT NULL,
  note VARCHAR,
  create_on TIMESTAMP NOT NULL,
  FOREIGN KEY(item_category) REFERENCES item_category(id)
);

CREATE TABLE geometry (
  id INTEGER NOT NULL PRIMARY KEY,
  lat REAL NOT NULL,
  lng REAL NOT NULL
);

CREATE TABLE location (
  item_id VARCHAR NOT NULL PRIMARY KEY,
  name VARCHAR NOT NULL,
  geometry INTEGER,
  FOREIGN KEY(item_id) REFERENCES item(id),
  FOREIGN KEY(geometry) REFERENCES geometry(id)
);

CREATE TABLE item_template (
  name VARCHAR NOT NULL,
  item_category INTEGER NOT NULL,
  PRIMARY KEY (name, item_category),
  FOREIGN KEY(item_category) REFERENCES item_category(id)
);

|]

convTypes :: [(String, TypeQ)]
convTypes =
        [ ("float", [t|Double|])
        , ("date", [t|Day|])
        , ("timestamp", [t|LocalTime|])
        , ("double", [t|Double|])
        , ("varchar", [t|String|])
        , ("integer", [t|Int|])
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

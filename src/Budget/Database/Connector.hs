module Budget.Database.Connector
  ( module Database.HDBC
  , module Database.HDBC.Sqlite3
  ) where

import           Database.HDBC (ConnWrapper(..), IConnection)
import           Database.HDBC.Sqlite3 (connectSqlite3)

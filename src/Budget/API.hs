{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Budget.API where

import           Servant
import           Control.Monad.Free
import           Budget.Core
import           Budget.Database

type BudgetAPI
  =    "categories" :> "income"  :> Get '[JSON] [Category]
  :<|> "categories" :> "expense" :> Get '[JSON] [Category]

api :: Proxy BudgetAPI
api = Proxy

server :: ConnWrapper -> Server BudgetAPI
server conn
  =    runDB (runStoreM (dispatch GetIncomeCategory)) conn
  :<|> runDB (runStoreM (dispatch GetExpenseCategory)) conn

createDB :: ConnWrapper -> IO ()
createDB conn = runDB (rawDB schema) conn

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Budget.API where

import           Servant
import           Budget.Core
import           Budget.Database

type BudgetAPI
  =    "categories" :> "income"  :> Get '[JSON] [Category]
  :<|> "categories" :> "expense" :> Get '[JSON] [Category]
  :<|> "incomes" :> ReqBody '[JSON] NewIncomeR :> Post '[] ()

api :: Proxy BudgetAPI
api = Proxy

server :: ConnWrapper -> Server BudgetAPI
server conn
  =    getIncomeCategory
  :<|> getExpenseCategory
  :<|> createIncomeCategory
  where
    getIncomeCategory = runDB (runStoreM (dispatch GetIncomeCategory)) conn
    getExpenseCategory = runDB (runStoreM (dispatch GetExpenseCategory)) conn
    createIncomeCategory r = runDB (runStoreM (dispatch (CreateIncome r))) conn 

createDB :: ConnWrapper -> IO ()
createDB conn = runDB (rawDB schema) conn

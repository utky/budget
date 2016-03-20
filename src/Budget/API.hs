{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Budget.API where

import           Servant
import           Budget.Core
import           Budget.Database
import           Budget.API.Data

type BudgetAPI
  =    "categories" :> "income"  :> Get '[JSON] [Category]
  :<|> "categories" :> "expense" :> Get '[JSON] [Category]
  :<|> "categories" :> "income" :> ReqBody '[JSON] Category :> Post '[] ()
  :<|> "categories" :> "expense" :> ReqBody '[JSON] Category :> Post '[] ()
  :<|> "incomes" :> Capture "month" Month :> Get '[JSON] [Income]
  :<|> "expenses" :> Capture "month" Month :> Get '[JSON] [Expense]
  :<|> "incomes" :> ReqBody '[JSON] NewIncomeR :> Post '[] ()
  :<|> "expenses" :> ReqBody '[JSON] NewExpenseR :> Post '[] ()
  :<|> "templates" :> "income"  :> Get '[JSON] [ItemTemplate]
  :<|> "templates" :> "expense"  :> Get '[JSON] [ItemTemplate]
  :<|> "templates" :> ReqBody '[JSON] NewItemTemplateR :> Post '[] ()

api :: Proxy BudgetAPI
api = Proxy

runDataEvent :: (MonadIO m) => ConnWrapper -> Event StoreM a -> m a
runDataEvent c = (flip runDB) c . runStoreM . dispatch

server :: ConnWrapper -> Server BudgetAPI
server conn
  =    getIncomeCategory
  :<|> getExpenseCategory
  :<|> createIncomeCategory
  :<|> createExpenseCategory
  :<|> queryIncome
  :<|> queryExpense
  :<|> createIncome
  :<|> createExpense
  :<|> queryIncomeTemplate
  :<|> queryExpenseTemplate
  :<|> createItemTemplate
  where
    runDataEvent' = runDataEvent conn
    getIncomeCategory = runDataEvent' GetIncomeCategory
    getExpenseCategory = runDataEvent' GetExpenseCategory
    createIncomeCategory c = runDataEvent' (CreateIncomeCategory c)
    createExpenseCategory c = runDataEvent' (CreateExpenseCategory c)
    queryIncome  (Month ((Year y), m)) = runDataEvent' (QueryIncome (ByMonth y m))
    queryExpense (Month ((Year y), m)) = runDataEvent' (QueryExpense (ByMonth y m))
    createIncome r = runDataEvent' (CreateIncome r)
    createExpense r = runDataEvent' (CreateExpense r)
    queryIncomeTemplate = runDataEvent' QueryIncomeTemplate
    queryExpenseTemplate = runDataEvent' QueryExpenseTemplate
    createItemTemplate r = runDataEvent' (CreateItemTemplate r)

createDB :: ConnWrapper -> IO ()
createDB conn = runDB (rawDB schema) conn

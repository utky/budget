{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-| Defines action in domain.
    
-}
module Budget.Core.Event where

import           GHC.Generics
import           Data.Aeson
import           Data.Text (Text)
import           Budget.Core.Data
import           Budget.Core.Store

data Event m a where
  GetIncomeCategory :: Event StoreM [Category]
  GetExpenseCategory :: Event StoreM [Category]
  CreateExpense :: NewExpenseR -> Event StoreM ()
  CreateIncome :: NewIncomeR -> Event StoreM ()
  CreateExpenseCategory :: Category -> Event StoreM ()
  CreateIncomeCategory :: Category -> Event StoreM ()

dispatch :: Event m a -> m a
dispatch GetIncomeCategory
  = liftS (Fetch incomeCategories)
dispatch GetExpenseCategory
  = liftS (Fetch expenseCategories)
dispatch (CreateExpense x)
  = liftS (New () (NewExpense x))
dispatch (CreateIncome x)
  = liftS (New () (NewIncome x))
dispatch (CreateExpenseCategory cat)
  = liftS (New () (NewExpenseCategory cat))
dispatch (CreateIncomeCategory cat)
  = liftS (New () (NewIncomeCategory cat))


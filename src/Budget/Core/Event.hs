{-# LANGUAGE GADTs #-}
{-| Defines action in domain.
    
-}
module Budget.Core.Event where

import           Budget.Core.Data
import           Budget.Core.Store

data Event m a where
  GetIncomeCategory :: Event StoreM [Category]
  GetExpenseCategory :: Event StoreM [Category]


dispatch :: Event m a -> m a
dispatch GetIncomeCategory = liftS (Fetch incomeCategories)
dispatch GetExpenseCategory = liftS (Fetch expenseCategories)


{-# LANGUAGE GADTs #-}
{-| Defines action in domain.
    
-}
module Budget.Core.Event where

import           Budget.Core.Data
import           Budget.Core.Store

data Event m a where
  GetIncomeCategory :: Event StoreM [Category]
  GetExpenseCategory :: Event StoreM [Category]
  CreateExpense :: NewExpenseR -> Event StoreM ()
  CreateIncome :: NewIncomeR -> Event StoreM ()
  CreateExpenseCategory :: Category -> Event StoreM ()
  CreateIncomeCategory :: Category -> Event StoreM ()
  QueryIncome :: ByMonth -> Event StoreM [Income]
  QueryExpense :: ByMonth -> Event StoreM [Expense]
  QueryIncomeTemplate :: Event StoreM [ItemTemplate]
  QueryExpenseTemplate :: Event StoreM [ItemTemplate]
  CreateItemTemplate :: NewItemTemplateR -> Event StoreM ()

dispatch :: Event m a -> m a
dispatch GetIncomeCategory
  = liftS (Fetch (IncomeCategories id))
dispatch GetExpenseCategory
  = liftS (Fetch (ExpenseCategories id))
dispatch (CreateExpense x)
  = liftS (New () (NewExpense x))
dispatch (CreateIncome x)
  = liftS (New () (NewIncome x))
dispatch (CreateExpenseCategory cat)
  = liftS (New () (NewExpenseCategory cat))
dispatch (CreateIncomeCategory cat)
  = liftS (New () (NewIncomeCategory cat))
dispatch (QueryIncome m)
  = liftS (Fetch (IncomeByMonth id m))
dispatch (QueryExpense m)
  = liftS (Fetch (ExpenseByMonth id m))
dispatch QueryIncomeTemplate
  = liftS (Fetch (IncomeTemplates id))
dispatch QueryExpenseTemplate
  = liftS (Fetch (ExpenseTemplates id))
dispatch (CreateItemTemplate x)
  = liftS (New () (NewItemTemplate x))

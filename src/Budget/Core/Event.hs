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

data NewExpenseR
  = NewExpenseR
  { newExpenseName :: Text
  , newExpenseDate :: Date
  , newExpenseNote :: Text
  , newExpenseAmount :: Amount
  , newExpenseCategoryId :: Integer
  } deriving (Generic)

instance FromJSON NewExpenseR
instance ToJSON NewExpenseR

data NewIncomeR
  = NewIncomeR
  { newIncomeName :: Text
  , newIncomeDate :: Date
  , newIncomeNote :: Text
  , newIncomeAmount :: Amount
  , newIncomeCategoryId :: Integer
  } deriving (Generic)

instance FromJSON NewIncomeR
instance ToJSON NewIncomeR

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
dispatch (CreateExpense (NewExpenseR name date note amount catId))
  = liftS (New () (NewExpense name date note amount catId))
dispatch (CreateIncome (NewIncomeR name date note amount catId))
  = liftS (New () (NewIncome name date note amount catId))
dispatch (CreateExpenseCategory cat)
  = liftS (New () (NewExpenseCategory cat))
dispatch (CreateIncomeCategory cat)
  = liftS (New () (NewIncomeCategory cat))


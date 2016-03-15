{-# LANGUAGE DeriveGeneric #-}
module Budget.Core.Data.Item where

import           GHC.Generics
import           Data.Aeson
import           Data.Text (Text)
import           Budget.Core.Data.Amount
import           Budget.Core.Data.Category
import           Budget.Core.Data.Date

data NewExpenseR
  = NewExpenseR
  { newExpenseName :: Text
  , newExpenseDate :: Date
  , newExpenseNote :: Text
  , newExpenseAmount :: Amount
  , newExpenseCategoryId :: Int
  } deriving (Generic)

instance FromJSON NewExpenseR
instance ToJSON NewExpenseR

data NewIncomeR
  = NewIncomeR
  { newIncomeName :: Text
  , newIncomeDate :: Date
  , newIncomeNote :: Text
  , newIncomeAmount :: Amount
  , newIncomeCategoryId :: Int
  } deriving (Generic)

instance FromJSON NewIncomeR
instance ToJSON NewIncomeR

{-| Basic properties for representing income and expense.
-}
data Item
  = Item
  { itemId   :: String -- ^ Identifier of item
  , itemName :: Text -- ^ Name of item
  , itemDate :: Date -- ^ Date of 
  , itemNote :: Text -- ^ Note
  , itemAmount :: Amount
  , itemCategory :: Category
  } deriving (Generic)

instance Eq Item where
  x == y = itemId x == itemId y

instance Ord Item where
  a `compare` b = itemAmount a `compare` itemAmount b


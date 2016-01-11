{-# LANGUAGE DeriveGeneric #-}
module Budget.Core.Data where

import           GHC.Generics
import           Data.Aeson
import           Data.Monoid
import           Data.Time
import           Data.Text (Text)

-- Primitives
-- ==================================================================

{-| Amount of income or expense.
-}
type Amount = Integer

type Date = Day

instance ToJSON Day where
    toJSON d = toJSON (show d)
    
instance FromJSON Day where
    parseJSON (String s)
      = case s of
          (y1:y2:y3:y4:'-':m1:m2:'-':d1:d1:[]) -> -- TODO: write parser
          _                                    -> mzero


mkDate :: Integer -> Int -> Int -> Date
mkDate = fromGregorian

data Category
  = Category
  { categoryId :: Integer
  , categoryName :: String
  } deriving (Generic)

instance Show Category where
  show = categoryName

instance Eq Category where
  (Category x _) == (Category y _) = x == y

instance FromJSON Category
instance ToJSON Category

{-| Basic properties for representing income and expense.
-}
data Item
  = Item
  { itemId   :: Integer -- ^ Identifier of item
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

{-| Income for budget.
-}
data Income
  = Income
  { incomeItem :: Item
  }

{-| Expense for budget.
-}
data Expense
  = Expense
  { expenseItem :: Item
  }

data Saving
  = Saving
  { amount :: Item
  }

-- Methods
-- ==================================================================


-- Tag types
-- ==================================================================

{-| Category of expense.
-}
data ExpenseClass
  = FixedCost
  | VariableCost 
  deriving (Eq)


-- Classes
-- ==================================================================

{- | Projection to amount of money
 -}
class ToAmount a where
  {- | Map instance to amount.
   -}
  toAmount :: a -> Amount

  {- | Sum all amount of collection of instance.
   -}
  sumAmount :: (Foldable f) => f a -> Amount
  sumAmount = getSum . foldMap (Sum . toAmount)

instance ToAmount Item where
  toAmount = itemAmount

instance ToAmount Income where
  toAmount = itemAmount . incomeItem

instance ToAmount Expense where
  toAmount = itemAmount . expenseItem


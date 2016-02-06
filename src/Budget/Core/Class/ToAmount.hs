module Budget.Core.Class.ToAmount where

import           Data.Monoid (Sum(..))
import           Budget.Core.Data.Amount
import           Budget.Core.Data.Item

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


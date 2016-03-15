module Budget.Core.Class.ToAmount where

import           Data.Monoid (Sum(..))
import           Budget.Core.Data.Amount
import           Budget.Core.Data.Item
import qualified Budget.Core.Data.Income as I
import qualified Budget.Core.Data.Expense as E

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

instance ToAmount I.Income where
  toAmount = I.amount

instance ToAmount E.Expense where
  toAmount = E.amount


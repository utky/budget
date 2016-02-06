{-# LANGUAGE DeriveGeneric #-}
module Budget.Core.Data
    ( module Budget.Core.Data.Amount
    , module Budget.Core.Data.Category
    , module Budget.Core.Data.Date
    , module Budget.Core.Data.Item
    ) where

import           GHC.Generics
import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Monoid
import           Data.Time
import           Budget.Core.Data.Amount
import           Budget.Core.Data.Category
import           Budget.Core.Data.Date
import           Budget.Core.Data.Item

-- Primitives
-- ==================================================================

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



{-# LANGUAGE DeriveGeneric #-}
module Budget.Core.Data
    ( module Budget.Core.Data.Amount
    , module Budget.Core.Data.Category
    , module Budget.Core.Data.Date
    , module Budget.Core.Data.Item
    , module Budget.Core.Data.ItemTemplate
    , module Budget.Core.Data.Income
    , module Budget.Core.Data.Expense
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
import           Budget.Core.Data.ItemTemplate (ItemTemplate, NewItemTemplateR(..), newItemTemplateName, newItemTemplateCategoryId)
import           Budget.Core.Data.Income (Income)
import           Budget.Core.Data.Expense (Expense)

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



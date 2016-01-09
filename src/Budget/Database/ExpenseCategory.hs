{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Budget.Database.ExpenseCategory where

import           Budget.Core (Category(..))
import           Database.Relational.Query
import           Budget.Database.Internal
import           Budget.Database.Schema (defineTable)

$(defineTable "expense_category")

instance Iso Category ExpenseCategory where
  to   (Category i n)     = ExpenseCategory i n
  from (ExpenseCategory i n) = Category i n

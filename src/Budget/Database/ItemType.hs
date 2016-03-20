{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Budget.Database.ItemType where

import           Budget.Database.Schema (defineTable)

$(defineTable "item_type")

data ItemTypeName = Income | Expense deriving (Eq, Ord)
instance Show ItemTypeName where
  show Income = "income"
  show Expense = "expense"



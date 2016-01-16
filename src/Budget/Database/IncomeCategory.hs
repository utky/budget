{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Budget.Database.IncomeCategory where

import           Budget.Core (Category(..))
import           Database.Relational.Query
import           Budget.Database.Internal
import           Budget.Database.Schema (defineTable)

$(defineTable "income_category")

instance Iso Category IncomeCategory where
  to   (Category i n)     = IncomeCategory i n
  from (IncomeCategory i n) = Category i n

insertFromCategory :: Category -> InsertQuery ()
insertFromCategory = insertQueryIncomeCategory . valueFromCategory

valueFromCategory :: Category -> Relation () IncomeCategory
valueFromCategory cat = relation . return $
  IncomeCategory |$| value (categoryId cat)
                 |*| value (categoryName cat)

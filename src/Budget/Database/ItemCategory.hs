{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Budget.Database.ItemCategory where

import           Budget.Core (Category(..))
import           Database.Relational.Query hiding (id')
import           Budget.Database.Internal
import           Budget.Database.Schema (defineTable)

$(defineTable "item_category")

instance Iso Category ItemCategory where
  to   (Category i n)     = ItemCategory i n 1
  from (ItemCategory i n t) = Category i n

queryById :: Int -> Relation () ItemCategory
queryById itemCategoryId = relation $ do
  i <- query itemCategory
  wheres $ i ! id' .=. value itemCategoryId
  return i

expenseCategory :: Relation () ItemCategory
expenseCategory = queryById 1

incomeCategory :: Relation () ItemCategory
incomeCategory = queryById 2

insertFromCategory :: Int -> Category -> InsertQuery ()
insertFromCategory itemType = insertQueryItemCategory . valueFromCategory itemType

valueFromCategory :: Int -> Category -> Relation () ItemCategory
valueFromCategory itemType cat = relation . return $
  ItemCategory |$| value (categoryId cat)
               |*| value (categoryName cat)
               |*| value itemType

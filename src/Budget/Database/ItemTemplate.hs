{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Budget.Database.ItemTemplate where

import           Data.Text (unpack, pack)
import qualified Budget.Core.Data.ItemTemplate as T
import qualified Budget.Database.ItemCategory as ItemCategory
import qualified Budget.Database.ItemType as ItemType
import           Budget.Database.Internal
import           Database.Relational.Query

import           Budget.Database.Schema (defineTable)

$(defineTable "item_template")

type ItemTemplate' = (ItemTemplate, ItemCategory.ItemCategory)

instance From T.ItemTemplate ItemTemplate' where
  from (t, c)
    = T.ItemTemplate
    { T.name = pack . name $ t
    , T.category = from c
    }

itemTemplateByType
  :: ItemType.ItemTypeName
  -> Relation () ItemTemplate'
itemTemplateByType itemTypeName = relation $ do
  i <- query itemTemplate
  c <- query ItemCategory.itemCategory
  t <- query ItemType.itemType
  on $ i ! itemCategory' .=. c ! ItemCategory.id'
  on $ c ! ItemCategory.itemType' .=. t ! ItemType.id'
  wheres $ t ! ItemType.name' .=. value (show itemTypeName)
  return (i >< c)

incomeTemplates :: Relation () ItemTemplate'
incomeTemplates = itemTemplateByType ItemType.Income

expenseTemplates :: Relation () ItemTemplate'
expenseTemplates = itemTemplateByType ItemType.Expense

insert :: T.NewItemTemplateR -> InsertQuery ()
insert = insertQueryItemTemplate . valueFromIncome
  where
    valueFromIncome :: T.NewItemTemplateR -> Relation () ItemTemplate
    valueFromIncome i = relation . return $ 
      ItemTemplate
           |$| value (unpack . T.newItemTemplateName $ i)
           |*| value (T.newItemTemplateCategoryId i)

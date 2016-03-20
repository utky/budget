{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Budget.Database.Item where

import           Prelude hiding (id)
import qualified Data.Maybe as MB
import           Data.Text (unpack, pack)
import           Data.Time (LocalTime)
import qualified Budget.Core as Core
import qualified Budget.Core.Data.Expense as E
import qualified Budget.Core.Data.Income as I
import           Database.Relational.Query
import           Budget.Database.Internal
import qualified Budget.Database.ItemCategory as ItemCategory
import qualified Budget.Database.ItemType as ItemType
import           Budget.Database.Schema (defineTable)

$(defineTable "item")

type Item' = (Item, ItemCategory.ItemCategory)

instance From E.Expense Item' where
  from (i, c)
    = E.Expense
    { E.id = id i
    , E.name = pack . name $ i
    , E.date = date i
    , E.note = pack . MB.fromMaybe "" . note $ i
    , E.amount = amount i
    , E.category = from c
    }

instance From I.Income Item' where
  from (i, c)
    = I.Income
    { I.id = id i
    , I.name = pack . name $ i
    , I.date = date i
    , I.note = pack . MB.fromMaybe "" . note $ i
    , I.amount = amount i
    , I.category = from c
    }

type ItemId = String
-- | 
itemByMonth
  :: ItemType.ItemTypeName
  -> (Core.Date, Core.Date)
  -> Relation () Item'
itemByMonth itemTypeName (fromDay, toDay) = relation $ do
  i <- query item
  c <- query ItemCategory.itemCategory
  t <- query ItemType.itemType
  on $ i ! itemCategory' .=. c ! ItemCategory.id'
  on $ c ! ItemCategory.itemType' .=. t ! ItemType.id'
  wheres $ t ! ItemType.name' .=. value (show itemTypeName)
  wheres $ i ! date' .>=. unsafeSQLiteDayValue fromDay
  wheres $ i ! date' .<. unsafeSQLiteDayValue toDay
  return (i >< c)

incomeByMonth :: (Core.Date, Core.Date) -> Relation () Item'
incomeByMonth = itemByMonth ItemType.Income

expenseByMonth :: (Core.Date, Core.Date) -> Relation () Item'
expenseByMonth = itemByMonth ItemType.Expense

insertFromExpense :: ItemId -> LocalTime -> Core.NewExpenseR -> InsertQuery ()
insertFromExpense itemId now = insertQueryItem . valueFromExpense
  where
    valueFromExpense :: Core.NewExpenseR -> Relation () Item
    valueFromExpense i = relation . return $ 
      Item |$| value itemId
           |*| value (Core.newExpenseCategoryId i)
           |*| unsafeSQLiteDayValue (Core.newExpenseDate i)
           |*| (value . unpack) (Core.newExpenseName i)
           |*| value (Core.newExpenseAmount i)
           |*| (value . Just . unpack) (Core.newExpenseNote i)
           |*| unsafeSQLiteTimeValue now

insertFromIncome :: ItemId -> LocalTime -> Core.NewIncomeR -> InsertQuery ()
insertFromIncome itemId now = insertQueryItem . valueFromIncome
  where
    valueFromIncome :: Core.NewIncomeR -> Relation () Item
    valueFromIncome i = relation . return $ 
      Item |$| value itemId
           |*| value (Core.newIncomeCategoryId i)
           |*| unsafeSQLiteDayValue (Core.newIncomeDate i)
           |*| (value . unpack) (Core.newIncomeName i)
           |*| value (Core.newIncomeAmount i)
           |*| (value . Just . unpack) (Core.newIncomeNote i)
           |*| unsafeSQLiteTimeValue now

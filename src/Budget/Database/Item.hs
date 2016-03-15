{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Budget.Database.Item where

import           Data.Text (unpack)
import           Data.Time (LocalTime)
import qualified Budget.Core as Core
import           Database.Relational.Query
import           Budget.Database.Internal
import qualified Budget.Database.ItemCategory as ItemCategory
import qualified Budget.Database.ItemType as ItemType
import           Budget.Database.Schema (defineTable)

$(defineTable "item")


type ItemId = String
type ItemTypeName = String

nextMonth :: Int -> Int
nextMonth m = (m + 1) `mod` 12

itemByMonth :: ItemTypeName -> Relation (Core.Date, Core.Date) Item
itemByMonth itemTypeName = relation' . placeholder $ \ph -> do
  let fromDate = ph ! fst'
      toDate = ph ! snd'
  i <- query item
  c <- query ItemCategory.itemCategory
  t <- query ItemType.itemType
  on $ i ! itemCategory' .=. c ! ItemCategory.id'
  on $ c ! ItemCategory.itemType' .=. t ! ItemType.id'
  wheres $ t ! ItemType.name' .=. value itemTypeName
  wheres $ i ! date' .>=. fromDate
  wheres $ i ! date' .<. toDate
  return i

incomeByMonth :: Relation (Core.Date, Core.Date) Item
incomeByMonth = itemByMonth "income"

expenseByMonth :: Relation (Core.Date, Core.Date) Item
expenseByMonth = itemByMonth "expense"

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

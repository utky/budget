{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Budget.Database.Item where

import           Data.Text (unpack)
import           Data.Time (LocalTime)
import qualified Budget.Core as Core
import           Database.Relational.Query
import           Budget.Database.Internal
import           Budget.Database.Schema (defineTable)

$(defineTable "item")

type ItemId = String

insertFromExpense :: ItemId -> LocalTime -> Core.NewExpenseR -> InsertQuery ()
insertFromExpense itemId now = insertQueryItem . valueFromExpense
  where
    valueFromExpense :: Core.NewExpenseR -> Relation () Item
    valueFromExpense i = relation . return $ 
      Item |$| value itemId
           |*| value 1
           |*| value (Core.newExpenseDate i)
           |*| (value . unpack) (Core.newExpenseName i)
           |*| value (Core.newExpenseAmount i)
           |*| (value . Just . unpack) (Core.newExpenseNote i)
           |*| value now


insertFromIncome :: ItemId -> LocalTime -> Core.NewIncomeR -> InsertQuery ()
insertFromIncome itemId now = insertQueryItem . valueFromIncome
  where
    valueFromIncome :: Core.NewIncomeR -> Relation () Item
    valueFromIncome i = relation . return $ 
      Item |$| value itemId
           |*| value 1
           |*| value (Core.newIncomeDate i)
           |*| (value . unpack) (Core.newIncomeName i)
           |*| value (Core.newIncomeAmount i)
           |*| (value . Just . unpack) (Core.newIncomeNote i)
           |*| value now

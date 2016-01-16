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

insertFromIncome :: Int -> LocalTime -> Core.NewIncomeR -> InsertQuery ()
insertFromIncome itemId now = insertQueryItem . valueFromIncome
  where
    valueFromIncome :: Core.NewIncomeR -> Relation () Item
    valueFromIncome i = relation . return $ 
      Item |$| value itemId
           |*| value (Core.newIncomeAmount i)
           |*| value 1
           |*| value (Core.newIncomeDate i)
           |*| (value . unpack) (Core.newIncomeName i)
           |*| (value . Just . unpack) (Core.newIncomeNote i)
           |*| value now

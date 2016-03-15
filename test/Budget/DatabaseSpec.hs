{-# LANGUAGE OverloadedStrings #-}
module Budget.DatabaseSpec (spec) where

import           Test.Hspec
import           Budget.Core
import           Budget.Database

connect :: IO ConnWrapper
connect = fmap ConnWrapper (connectSqlite3 ":memory:")

createSchema :: DB ()
createSchema = rawDB schema

runDB' :: (IConnection conn) => conn -> DB a -> IO a
runDB' conn db = runDB (createSchema >> db) conn

spec :: Spec
spec =
  describe "Database" $ do

    describe "DB" $ do

      it "can create schema" $ do
        conn <- connect
        names <- runDB' conn tableNames
        (0 <= length names) `shouldBe` True

      it "can insert new category" $ do
        conn <- connect
        cats <- runDB' conn $
          runStoreM $ 
            liftS (New () (NewExpenseCategory (Category 1 "hoge")))
              >> liftS (Fetch (ExpenseCategories id))

        (categoryName $ head cats) `shouldBe` "hoge"

      it "can insert new income" $ do
        let r = NewIncomeR 
                { newIncomeName = "name"
                , newIncomeDate = mkDate 2016 1 1
                , newIncomeNote = "note"
                , newIncomeAmount = 1
                , newIncomeCategoryId = 1
                }
        conn <- connect
        incomes <- runDB' conn $
          runStoreM $ 
            liftS (New () (NewIncome r))
              >> liftS (Fetch (IncomeByMonth id (ByMonth 2016 1)))

        (length incomes) `shouldBe` 1

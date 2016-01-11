{-# LANGUAGE OverloadedStrings #-}
module Budget.DatabaseSpec (spec) where

import           Test.Hspec
import           Budget.Core
import           Budget.Database

connect :: IO ConnWrapper
connect = fmap ConnWrapper (connectSqlite3 ":memory:")

createSchema :: DB ()
createSchema = rawDB schema

runDB' :: (IConnection conn) => DB a -> conn -> IO a
runDB' db = runDB (createSchema >> db)

spec :: Spec
spec =
  describe "Database" $ do

    describe "DB" $ do

      it "can create schema" $ do
        conn <- connect
        names <- runDB' tableNames conn
        (0 <= length names) `shouldBe` True

      it "can insert new category" $ do
        conn <- connect
        runDB' (New (NewExpenseCategory (Category 1 "hoge"))) conn
        (0 <= length names) `shouldBe` True


{-# LANGUAGE OverloadedStrings #-}
module Budget.Core.DataSpec where

import           Test.Hspec
import           Budget.Core

testItem :: Item
testItem = Item
         { itemName = "foo"
         , itemDate = mkDate 2016 1 1
         , itemAmount = 100
         , itemNote = "bar"
         , itemCategory = Category 1 "blha"
         }

spec :: Spec
spec =
  describe "Data" $ do

    describe "Category" $ do
      it "can serializable to JSON" $ do
        let d = Category { categoryId = 1, categoryName = "Foo" }
        (decode . encode) d `shouldBe` (Just d)

    describe "Item" $ do
      it "can accumlate its amount" $ do
        let item x = testItem { itemAmount = x }
            amounts = [ 100, 100, 100 ]
        sumAmount (map item amounts) `shouldBe` sum amounts

    describe "Income" $ do
      it "can accumlate its amount" $ do
        let income x = Income (testItem { itemAmount = x })
            amounts = [ 100, 100, 100 ]
        sumAmount (map income amounts) `shouldBe` sum amounts

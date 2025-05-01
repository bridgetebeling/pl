-- test/DailyTwoSpec.hs

module Main (main) where

import Test.Hspec
import DailyTwo

main :: IO ()
main = hspec $ do
  describe "every4th" $ do
    it "returns every 4th element from a list of Ints" $ do
      every4th [1..10] `shouldBe` [4,8]
    it "works with empty list" $ do
      every4th ([] :: [Int]) `shouldBe` []

  describe "tupleDotProduct" $ do
    it "computes dot product of two lists" $ do
      tupleDotProduct [1,2,3] [4,5,6] `shouldBe` 32
    it "works with empty lists" $ do
      tupleDotProduct [] [] `shouldBe` 0

  describe "appendToEach" $ do
    it "appends a string to each element" $ do
      appendToEach "!!!" ["Hi", "Bye"] `shouldBe` ["Hi!!!", "Bye!!!"]
    it "returns empty list when given empty list" $ do
      appendToEach "test" [] `shouldBe` []

  describe "toSetList" $ do
    it "removes duplicates and sorts list" $ do
      toSetList [5,1,2,3,3,4,5,5] `shouldBe` [1,2,3,4,5]
    it "works with already unique sorted list" $ do
      toSetList [1,2,3] `shouldBe` [1,2,3]
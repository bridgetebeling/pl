-- test/DailySixSpec.hs
module Main where

import Test.Hspec
import DailySix

main :: IO ()
main = hspec $ do
  describe "shorterThan" $ do
    it "filters words longer than given number" $ do
      shorterThan 4 ["egg", "cupcake", "bun", "whisk"] `shouldBe` ["egg", "bun"]
    it "returns empty when no word matches" $ do
      shorterThan 2 ["baking", "oven"] `shouldBe` []

  describe "removeMultiples" $ do
    it "removes multiples of 5" $ do
      removeMultiples 5 [2, 5, 10, 13, 25] `shouldBe` [2, 13]
    it "works with empty list" $ do
      removeMultiples 3 [] `shouldBe` []

  describe "onlyJust" $ do
    it "removes all Nothing values" $ do
      onlyJust [Nothing, Just 350, Nothing, Just 180] `shouldBe` [Just 350, Just 180]
    it "returns same list if all values are Just" $ do
      onlyJust [Just 200, Just 175] `shouldBe` [Just 200, Just 175]
    it "returns empty list if all values are Nothing" $ do
      onlyJust [Nothing, Nothing] `shouldBe` ([] :: [Maybe Int])
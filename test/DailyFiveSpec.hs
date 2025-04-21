-- File: test/DailyFiveSpec.hs
module Main where

import Test.Hspec
import DailyFive

main :: IO ()
main = hspec $ do
  describe "multPairs" $ do
    it "multiplies pairs of numbers like 7 * 2 or 0 * 4" $ do
      multPairs [(7,2), (0,4), (3,5)] `shouldBe` [14, 0, 15]
    it "gives an empty list if input is empty" $ do
      multPairs [] `shouldBe` []

  describe "squareList" $ do
    it "squares each number and pairs them up nicely" $ do
      squareList [5, 9, 1] `shouldBe` [(5,25), (9,81), (1,1)]
    it "returns an empty list for an empty input list" $ do
      squareList [] `shouldBe` []

  describe "findLowercase" $ do
    it "says True if the string starts with lowercase, like 'pencil'" $ do
      findLowercase ["pencil", "iPhone", "laptop", "TV"] `shouldBe` [True, True, True, False]
    it "handles a one-letter lowercase and one-letter uppercase case" $ do
      findLowercase ["x", "Z"] `shouldBe` [True, False]
    it "returns an empty list when input is empty" $ do
      findLowercase [] `shouldBe` []

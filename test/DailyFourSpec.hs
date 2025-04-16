-- File: test/DailyFourSpec.hs

module Main (main) where

import Test.Hspec
import Test.HUnit
import Control.Exception (evaluate)
import DailyFour

main :: IO ()
main = hspec $ do

  -- zip3Lists tests
  describe "zip3Lists" $ do
    it "zips three equal-length lists correctly" $ do
      zip3Lists [1, 2, 3] ['a', 'b', 'c'] [4, 5, 6] `shouldBe` [(1, 'a', 4), (2, 'b', 5), (3, 'c', 6)]
    it "zips empty lists into an empty list" $ do
      zip3Lists [] [] [] `shouldBe` ([] :: [(Int, Char, Int)])
    it "throws an error if the lists are not the same length" $ do
      evaluate (zip3Lists [1] [2] []) `shouldThrow` anyErrorCall

  -- unzipTriples tests
  describe "unzipTriples" $ do
    it "unzips list of triples into three lists" $ do
      unzipTriples [(1,2,3), (4,5,6), (7,8,9)] `shouldBe` ([1,4,7], [2,5,8], [3,6,9])
    it "works on an empty list" $ do
      unzipTriples ([] :: [(Int, Int, Int)]) `shouldBe` ([], [], [])
    it "works on a single triple" $ do
      unzipTriples [(10,20,30)] `shouldBe` ([10], [20], [30])

  -- mergeSorted3 tests
  describe "mergeSorted3" $ do
    it "merges three sorted lists into one sorted list" $ do
      mergeSorted3 [2, 3, 5] [1, 8] [-1, 0, 4, 10] `shouldBe` [-1,0,1,2,3,4,5,8,10]
    it "works if all the lists are empty" $ do
      mergeSorted3 [] [] [] `shouldBe` ([] :: [Int])
    it "works if one list is empty" $ do
      mergeSorted3 [1,3] [] [2,4] `shouldBe` [1,2,3,4]

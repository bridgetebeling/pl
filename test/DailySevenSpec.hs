-- test/DailySevenSpec.hs
module Main where

import Test.Hspec
import DailySeven

spec :: Spec
spec = do
  describe "findLongest" $ do
    it "finds the word that's like the biggest cake at the bake-off" $ do
      findLongest ["pie", "croissant", "bun"] `shouldBe` "croissant"
    it "returns an empty plate if no cakes (empty list)" $ do
      findLongest [] `shouldBe` ""

  describe "anyLarger" $ do
    it "returns True when any oven temperature is hot enough" $ do
      anyLarger 350 [250,300,375,200] `shouldBe` True
    it "returns False when none of the ovens are preheated enough" $ do
      anyLarger 350 [100,200,300] `shouldBe` False
    it "handles empty tray (empty list) properly" $ do
      anyLarger 350 [] `shouldBe` False

  describe "allNames" $ do
    it "combines all the chefs' names into one recipe list" $ do
      allNames [("sugar", "plum"), ("ginger", "snap")] `shouldBe` "sugar plum, ginger snap"
    it "returns an empty mixing bowl when given no ingredients (empty list)" $ do
      allNames [] `shouldBe` ""

main :: IO ()
main = hspec spec

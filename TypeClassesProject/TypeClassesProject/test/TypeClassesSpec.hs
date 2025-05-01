-- test/TypeClassesSpec.hs

module Main (main) where

import Test.Hspec
import TypeClasses

main :: IO ()
main = hspec $ do
  describe "Show instance for Vec" $ do
    it "formats Vec [1.0, 2.0, 3.0, 4.0] correctly" $ do
      show (Vec [1.0, 2.0, 3.0, 4.0]) `shouldBe` "Vec [1.0,2.0,3.0,4.0]"

  describe "Num instance for Vec" $ do
    it "adds vectors element by element" $ do
      (Vec [1,2,3]) + (Vec [4,5,6]) `shouldBe` Vec [5,7,9]

    it "multiplies vectors element by element" $ do
      (Vec [1,2,3]) * (Vec [4,5,6]) `shouldBe` Vec [4,10,18]

    it "handles fromInteger with infinite list, trimmed with take" $ do
      let (Vec xs) = fromInteger 7 :: Vec
      take 3 xs `shouldBe` [7.0, 7.0, 7.0]

  describe "Eq instance for Vec" $ do
    it "returns True for equal Vecs" $ do
      Vec [1,2,3] == Vec [1,2,3] `shouldBe` True

    it "returns False for unequal Vecs" $ do
      Vec [1,2,3] == Vec [3,2,1] `shouldBe` False

  describe "Ord instance for Vec" $ do
    it "compares using standard list ordering" $ do
      compare (Vec [1,2,3]) (Vec [1,2,4]) `shouldBe` LT

  describe "VecT instance and magnitude" $ do
    it "calculates magnitude correctly" $ do
      magnitude (Vec [3,4]) `shouldBe` 5.0

  describe "Semigroup and Monoid instances for Vec" $ do
    it "Semigroup: adds Vecs with <>" $ do
      (Vec [1,2]) <> (Vec [3,4]) `shouldBe` Vec [4,6]

    it "Monoid: mempty is zero Vec (check with take)" $ do
      let (Vec xs) = mempty :: Vec
      take 3 xs `shouldBe` [0.0, 0.0, 0.0]

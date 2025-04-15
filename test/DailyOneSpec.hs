module DailyOneSpec where

import Test.Hspec
import DailyOne

spec :: Spec
spec = do
  describe "quadratic" $ do
    it "quadratic 0 0 0 1 = 0" $
      quadratic 0 0 0 1 `shouldBe` 0

    it "quadratic 1 2 3 3 = 34" $
      quadratic 1 2 3 3 `shouldBe` 34

  describe "scaleVector" $ do
    it "scaleVector 2 (3,4) = (6,8)" $
      scaleVector 2 (3, 4) `shouldBe` (6, 8)

  describe "tripleDistance" $ do
    it "distance between origin and (1,2,2) is 3.0" $
      tripleDistance (0, 0, 0) (1, 2, 2) `shouldBe` 3.0

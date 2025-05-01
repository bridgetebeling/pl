-- File: test/Spec.hs
module Main (main) where

import qualified DailyFiveSpec
import qualified TriTreeSpec

import Test.Hspec (hspec)

main :: IO ()
main = do
  DailyFiveSpec.main
  hspec TriTreeSpec.spec

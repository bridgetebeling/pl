-- File: src/DailyFive.hs
module DailyFive where

import Data.Char (isLower)

-- | multPairs
-- Takes a list of (Int, Int) tuples and returns a list of the products.
-- This is useful for processing paired integer data with multiplication.
multPairs :: [(Int, Int)] -> [Int]
multPairs = map (\(x, y) -> x * y)

-- | squareList
-- Takes a list of Ints and gives back tuples of the number and its square.
-- Helpful for creating quick mappings of values and their squares.
squareList :: [Int] -> [(Int, Int)]
squareList = map (\x -> (x, x * x))

-- | findLowercase
-- Takes a list of Strings and returns True for each string that starts with a lowercase letter.
-- Uses isLower from Data.Char and assumes each string is non-empty.
findLowercase :: [String] -> [Bool]
findLowercase = map check
  where
    check (x:_) = isLower x
    check []    = False


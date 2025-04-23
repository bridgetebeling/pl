-- src/DailySix.hs
module DailySix where

-- shorterThan :: Int -> [String] -> [String]
-- Takes a number and a list of words, returns only those words
-- that are shorter than or equal to the number given.
shorterThan :: Int -> [String] -> [String]
shorterThan n words = filter (\w -> length w <= n) words

-- removeMultiples :: Int -> [Int] -> [Int]
-- Takes a number and a list of integers, returns a list
-- with all the numbers that are NOT multiples of the given number.
removeMultiples :: Int -> [Int] -> [Int]
removeMultiples n nums = filter (\x -> x `mod` n /= 0) nums

-- onlyJust :: [Maybe a] -> [Maybe a]
-- Filters a list of Maybe values, keeping only the Just values.
onlyJust :: [Maybe a] -> [Maybe a]
onlyJust = filter isJust
  where isJust (Just _) = True
        isJust Nothing  = False

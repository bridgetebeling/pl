-- File: src/DailyFour.hs

module DailyFour where

-- zip3Lists
-- Type: [a] -> [b] -> [c] -> [(a, b, c)]
-- This function takes three lists of the same length and returns a list of triples,
-- where each triple has one item from each list at the same position.
zip3Lists :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3Lists [] [] [] = []
zip3Lists (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3Lists xs ys zs
zip3Lists _ _ _ = error "All input lists must be the same length."

-- unzipTriples
-- Type: [(a, b, c)] -> ([a], [b], [c])
-- This function takes a list of triples and separates them into three lists,
-- grouping the first, second, and third elements respectively.
unzipTriples :: [(a, b, c)] -> ([a], [b], [c])
unzipTriples [] = ([], [], [])
unzipTriples ((x, y, z):xs) =
  let (xs1, ys1, zs1) = unzipTriples xs
  in (x:xs1, y:ys1, z:zs1)

-- mergeSorted2
-- Type: Ord a => [a] -> [a] -> [a]
-- Helper function to merge two sorted lists into one sorted list.
-- Uses recursion to compare head values and build result list.
mergeSorted2 :: Ord a => [a] -> [a] -> [a]
mergeSorted2 xs [] = xs
mergeSorted2 [] ys = ys
mergeSorted2 (x:xs) (y:ys)
  | x <= y    = x : mergeSorted2 xs (y:ys)
  | otherwise = y : mergeSorted2 (x:xs) ys

-- mergeSorted3
-- Type: Ord a => [a] -> [a] -> [a] -> [a]
-- Takes 3 sorted lists and returns a single sorted list.
-- Uses mergeSorted2 helper to combine them without using built-ins.
mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
mergeSorted3 xs ys zs = mergeSorted2 (mergeSorted2 xs ys) zs

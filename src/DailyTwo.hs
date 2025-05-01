-- src/DailyTwo.hs

module DailyTwo where

-- | every4th :: [a] -> [a]
-- Takes a list and returns a new list containing every fourth element.
-- Example: every4th [1..10] == [4,8]
every4th :: [a] -> [a]
every4th = go 1
  where
    go _ [] = []
    go n (x:xs)
      | n `mod` 4 == 0 = x : go (n + 1) xs
      | otherwise      = go (n + 1) xs

-- | tupleDotProduct :: Num a => [a] -> [a] -> a
-- Takes two lists of numbers and returns their dot product.
-- Example: tupleDotProduct [1,2,3] [4,5,6] == 32
tupleDotProduct :: Num a => [a] -> [a] -> a
tupleDotProduct [] [] = 0
tupleDotProduct (x:xs) (y:ys) = x * y + tupleDotProduct xs ys
-- | appendToEach :: String -> [String] -> [String]

-- Appends a string to each string in the list.
-- Example: appendToEach "!!!" ["Hello", "Goodbye"] == ["Hello!!!", "Goodbye!!!"]
appendToEach :: String -> [String] -> [String]
appendToEach _ [] = []
appendToEach s (x:xs) = (x ++ s) : appendToEach s xs

-- | toSetList :: (Eq a, Ord a) => [a] -> [a]
-- Removes duplicates and returns a list with unique elements only.
-- Example: toSetList [5,1,2,3,3,4,5,5] == [1,2,3,4,5]
toSetList :: (Eq a, Ord a) => [a] -> [a]
toSetList = dedup . qsort
  where
    dedup [] = []
    dedup [x] = [x]
    dedup (x:y:xs)
      | x == y    = dedup (y:xs)
      | otherwise = x : dedup (y:xs)

    qsort [] = []
    qsort (x:xs) = qsort [a | a <- xs, a < x] ++ [x] ++ qsort [a | a <- xs, a >= x]
-- src/DailySeven.hs
module DailySeven where

-- | findLongest
--   Type: [String] -> String
--   Params: a list of tasty words (strings)
--   Result: returns the first longest word (like finding the biggest cupcake), or empty string if list is empty
findLongest :: [String] -> String
findLongest = foldl compareWords ""
  where
    compareWords acc word
      | length word > length acc = word
      | otherwise = acc

-- | anyLarger
--   Type: Int -> [Int] -> Bool
--   Params: a number (think oven temperature) and a list of numbers
--   Result: True if any number is as hot or hotter than the given one, False otherwise
anyLarger :: Int -> [Int] -> Bool
anyLarger n = foldl checkLarger False
  where
    checkLarger acc x = acc || x >= n

-- | allNames
--   Type: [(String, String)] -> String
--   Params: a list of (firstName, lastName) pairs (like recipe names)
--   Result: a single string of all full names stirred together with commas
allNames :: [(String, String)] -> String
allNames = foldr appendName ""
  where
    appendName (first, last) "" = first ++ " " ++ last
    appendName (first, last) acc = first ++ " " ++ last ++ ", " ++ acc


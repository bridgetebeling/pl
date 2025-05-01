module Encoder where

-- | removeChar
-- Gets rid of all occurrences of a character from a string.
-- Params: the char to remove, and the string to filter.
-- Returns: the string without that character.
removeChar :: Char -> String -> String
removeChar _ [] = []
removeChar c (x:xs)
  | c == x    = removeChar c xs
  | otherwise = x : removeChar c xs

-- | removeWhitespace
-- Filters out all spaces, tabs, newlines, and carriage returns.
-- Just composes a bunch of removeChar calls.
removeWhitespace :: String -> String
removeWhitespace = removeChar ' ' . removeChar '\t' . removeChar '\n' . removeChar '\r'

-- | removePunctuation
-- Removes punctuation like commas, brackets, braces, etc.
-- Done by composing removeChar with each one.
removePunctuation :: String -> String
removePunctuation = removeChar ',' . removeChar '.' . removeChar '(' . removeChar ')' . removeChar '[' . removeChar ']' . removeChar '{' . removeChar '}'

-- | charsToAscii
-- Converts a string into a list of ASCII integer codes.
charsToAscii :: String -> [Int]
charsToAscii [] = []
charsToAscii (x:xs) = fromEnum x : charsToAscii xs

-- | asciiToChars
-- Converts a list of ASCII integers back into a string.
asciiToChars :: [Int] -> String
asciiToChars [] = []
asciiToChars (x:xs) = toEnum x : asciiToChars xs

-- | shiftInts
-- Adds a shift to every int in a list, wrapping with mod 128.
shiftInts :: Int -> [Int] -> [Int]
shiftInts _ [] = []
shiftInts n (x:xs) = ((x + n) `mod` 128) : shiftInts n xs

-- | shiftMessage
-- Takes a string and a shift amount, and moves each char by that amount.
-- It's like a Caesar cipher but with full ASCII.
shiftMessage :: Int -> String -> String
shiftMessage n = asciiToChars . shiftInts n . charsToAscii

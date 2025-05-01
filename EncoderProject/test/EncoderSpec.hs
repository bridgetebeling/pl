module Main (main) where

import Test.Hspec
import Encoder

main :: IO ()
main = hspec $ do
  describe "removeChar" $ do
    it "removes a given character from a string" $ do
      removeChar 'a' "banana" `shouldBe` "bnn"
    it "works with empty string" $ do
      removeChar 'x' "" `shouldBe` ""

  describe "removeWhitespace" $ do
    it "removes all whitespace chars from string" $ do
      removeWhitespace "a b\tc\nd\re" `shouldBe` "abcde"

  describe "removePunctuation" $ do
    it "removes punctuation chars from string" $ do
      removePunctuation "Hello, world. (test) [data] {info}" `shouldBe` "Hello world test data info"

  describe "charsToAscii" $ do
    it "converts chars to ASCII ints" $ do
      charsToAscii "abc" `shouldBe` [97,98,99]

  describe "asciiToChars" $ do
    it "converts ASCII ints to chars" $ do
      asciiToChars [97,98,99] `shouldBe` "abc"

  describe "shiftInts" $ do
    it "shifts each int by n mod 128" $ do
      shiftInts 1 [2,4,6,127] `shouldBe` [3,5,7,0]

  describe "shiftMessage" $ do
    it "shifts message characters by n (ASCII shift)" $ do
      shiftMessage 1 "abc" `shouldBe` "bcd"
    it "works with negative shift" $ do
      shiftMessage (-1) "bcd" `shouldBe` "abc"

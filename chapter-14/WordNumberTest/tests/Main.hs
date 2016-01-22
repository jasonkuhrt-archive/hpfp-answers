module Main where

import Test.Hspec
import WordNumber (digitToWord, digits, wordNumber)



main :: IO ()
main = hspec $ do



  describe "digitToWord" $ do

    it "returns zero for 0" $
      shouldBe "zero" (digitToWord 0)

    it "returns one for 1" $
      shouldBe "one" (digitToWord 1)



  describe "wordNumber" $ do

    it "returns one-zero-zero for 100" $
      shouldBe "one-zero-zero" (wordNumber 100)

    it "returns nine-zero-zero-one for 9001" $
      shouldBe "nine-zero-zero-one" (wordNumber 9001)



  describe "digits" $ do

    it "returns [1] for 1" $
      shouldBe [1] (digits 1)

    it "returns [1,2,3] for 123" $
      shouldBe [1,2,3] (digits 123)

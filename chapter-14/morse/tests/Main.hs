module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck



main :: IO ()
main = quickCheck thereAndBackAgain



-- Properties

-- One property is that we should be able to convert a character into
-- morse code and then back from that morse code into the original character.

thereAndBackAgain :: Property
thereAndBackAgain = forAll charGen test
  where
  test :: Char -> Bool
  test char = Just char == (charToMorse char >>= morseToChar)



-- Generators

charGen :: Gen Char
charGen = elements charsAllowed
  where
  charsAllowed :: String
  charsAllowed = M.keys charCodeMap

codeGen :: Gen Morse
codeGen = elements codesAllowed
  where
  codesAllowed :: [Morse]
  codesAllowed = M.elems charCodeMap

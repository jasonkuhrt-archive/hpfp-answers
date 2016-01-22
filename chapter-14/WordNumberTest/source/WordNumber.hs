module WordNumber where

import Data.List (intercalate)



digitToWord :: Integer -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "unknown"



readLastDigit :: Integral a => a -> a
readLastDigit n = mod n 10

dropLastDigit :: Integral a => a -> a
dropLastDigit n = div n 10

digits :: Integer -> [Integer]
digits number = doDigits number []
  where
  doDigits n acc
    | n < 10    = n : acc
    | otherwise = doDigits (dropLastDigit n) (readLastDigit n : acc)



wordNumber :: Integer -> String
wordNumber = intercalate "-" . map digitToWord . digits

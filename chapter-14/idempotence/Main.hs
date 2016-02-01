module Main where

import Test.QuickCheck
import Data.Char
import Data.List



main :: IO ()
main = do
  quickCheck capitalizeWordPropIdempotent
  quickCheck sortPropIdempotent



-- 1 ~ capitalizeWord is idempotent

capitalizeWordPropIdempotent :: Property
capitalizeWordPropIdempotent = forAll generator test
  where
  generator = arbitrary :: Gen String
  test word = x1 == x2 && x1 == x4
    where
    x1 = capitalizeWord word
    x2 = repeats2 capitalizeWord $ word
    x4 = repeats4 capitalizeWord $ word

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (char:string) = toUpper char : string



-- 2 ~ sort is idempotent

sortPropIdempotent :: Property
sortPropIdempotent = forAll generator test
  where
  generator = arbitrary :: Gen [Integer]
  test list = x1 == x2 && x1 == x4
    where
    x1 = sort list
    x2 = repeats2 sort $ list
    x4 = repeats4 sort $ list






repeats2 f = f . f

repeats4 = repeats2 . repeats2

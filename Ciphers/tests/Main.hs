module Main where

import Test.QuickCheck
import Caesar



main :: IO ()
main =
  quickCheck caesarInOutProp



caesarInOutProp :: Property
caesarInOutProp = forAll generator test
  where
  generator = sublistOf chars
  test string = string == (decrypt 3 . encrypt 3 $ string )

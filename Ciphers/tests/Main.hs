module Main where

import Test.QuickCheck
import qualified Caesar
import qualified Vigenere



main :: IO ()
main = do
  quickCheck caesarInOutProp
  quickCheck vigenereInOutProp
  quickCheck vigenereInOutProp2



caesarInOutProp :: Property
caesarInOutProp = forAll generator test
  where
  generator = sublistOf Caesar.chars
  test string = string == (Caesar.decrypt 3 . Caesar.encrypt 3 $ string)

vigenereInOutProp :: Property
vigenereInOutProp = forAll generator test
  where
  generator = sublistOf Vigenere.pool
  test string = string == (Vigenere.decrypt "FOO" . Vigenere.encrypt "FOO" $ string)

vigenereInOutProp2 :: Property
vigenereInOutProp2 = forAll generator test
  where
  generator = sublistOf Vigenere.pool
  test string = "FOO" == (Vigenere.decrypt string . Vigenere.encrypt string $ "FOO")

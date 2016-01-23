module QuickCheckTests where

import Test.QuickCheck



main :: IO ()
main = do
  quickCheck halfDoubledIdentity
  quickCheck twoHalvesIdentity




halfDoubledIdentity = forAll generator test
  where
  generator = arbitrary :: Gen Float
  test n = n == (half n * 2)

twoHalvesIdentity = forAll generator test
  where
  generator = arbitrary :: Gen Float
  test n = n == (half n + half n)

half :: Float -> Float
half n = n / 2

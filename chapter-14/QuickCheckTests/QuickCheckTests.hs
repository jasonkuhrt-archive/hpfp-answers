module QuickCheckTests where

import Test.QuickCheck
import Data.List



main :: IO ()
main = do
  quickCheck halfAddHalfIdentProp
  quickCheck halfAddHalfIdentityProp
  quickCheck sortOrdersListProp
  quickCheck plusAssociativeProp
  quickCheck plusCommutativeProp



-- 1 -- half

halfAddHalfIdentProp = forAll generator test
  where
  generator = arbitrary :: Gen Float
  test n = ((== n) . (* 2) . half) n

halfAddHalfIdentityProp = forAll generator test
  where
  generator = arbitrary :: Gen Float
  test n = n == (half n + half n)

half :: Float -> Float
half n = n / 2



-- 2 -- sort

sortOrdersListProp = forAll generator test
  where
  generator = arbitrary :: Gen [Float]
  test = isListOrdered . sort

isListOrdered :: Ord a => [a] -> Bool
isListOrdered xs = snd $ foldr go (Nothing, True) xs
  where
  go _ status@(_, False)      = status
  go x (Just xPrevious, flag) = (Just x, xPrevious >= x)
  go x (Nothing, flag)        = (Just x, flag) -- first iteration



-- 3 -- plus

plusAssociativeProp = forAll generator test
  where
  generator = arbitrary :: Gen (Integer, Integer, Integer)
  test (x, y, z) =
    (==) (x + (y + z)) ((x + y) + z)

plusCommutativeProp = forAll generator test
  where
  generator = arbitrary :: Gen (Integer, Integer)
  test (x, y) =
    (==) (x + y) (y + x)

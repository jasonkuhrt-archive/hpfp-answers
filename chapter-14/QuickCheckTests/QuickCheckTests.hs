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
  quickCheck multiplyAssociativeProp
  quickCheck multiplyCommutativeProp
  quickCheck quotRemLawProp
  quickCheck divModLawProp
  quickCheck powAssociativeProp
  quickCheck powCommutativeProp
  quickCheck reverseTwiceIsIdProp



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
  generator = arbitrary :: Gen (Float, Float)
  test (x, y) =
    (==) (x + y) (y + x)



-- 4 -- multiply

multiplyAssociativeProp = forAll generator test
  where
  generator = arbitrary :: Gen (Integer, Integer, Integer)
  test (x, y, z) =
    (==) (x * (y * z)) ((x * y) * z)

multiplyCommutativeProp = forAll generator test
  where
  generator = arbitrary :: Gen (Float, Float)
  test (x, y) =
    (==) (x * y) (y * x)



-- 5 -- quot/rem & div/mod laws

quotRemLawProp = forAll generator test
  where
  generator = suchThat (arbitrary :: Gen (Integer, Integer)) nonZeroDivisor
  test (x, y) = x == quot x y * y + rem x y

divModLawProp = forAll generator test
  where
  generator = suchThat (arbitrary :: Gen (Integer, Integer)) nonZeroDivisor
  test (x, y) = x == div x y * y + mod x y

nonZeroDivisor :: (Integer, Integer) -> Bool
nonZeroDivisor (_, denom) = denom /= 0



-- 6 -- Is `^` associative? commutative?

-- No, it is not!
powAssociativeProp = forAll generator test
  where
  generator = suchThat (arbitrary :: Gen (Integer, Integer, Integer)) nonNegativeExpos
  test (x, y, z) = (x ^ (y ^ z)) == ((x ^ y) ^ z)


-- No, it is not!
powCommutativeProp = forAll generator test
  where
  generator = suchThat (arbitrary :: Gen (Integer, Integer)) nonNegativeExpo
  test (x, y) = (x ^ y) /= (y ^ x)


nonNegativeExpos :: (Integer, Integer, Integer) -> Bool
nonNegativeExpos (_, y, z) = y >= 0 && z >= 0

nonNegativeExpo :: (Integer, Integer) -> Bool
nonNegativeExpo (x, y) = x >= 0 && y >= 0



-- 7 -- reverse . reverse == id

reverseTwiceIsIdProp = forAll generator test
  where
  generator = arbitrary :: Gen [String]
  test string = (reverse . reverse $ string) == id string



-- 8 --

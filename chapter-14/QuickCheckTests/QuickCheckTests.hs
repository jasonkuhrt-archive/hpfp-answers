module QuickCheckTests where

import Test.QuickCheck
import Data.List



main :: IO ()
main = do
  quickCheck halfAddHalfIdentProp
  quickCheck halfAddHalfIdentityProp
  quickCheck orderedListProp



-- 1 Half

halfAddHalfIdentProp = forAll generator test
  where
  generator = arbitrary :: Gen Float
  test n = n == (half n * 2)

halfAddHalfIdentityProp = forAll generator test
  where
  generator = arbitrary :: Gen Float
  test n = n == (half n + half n)

half :: Float -> Float
half n = n / 2



-- 2

-- TODO Manually write orderedList
-- TODO Type-level generator of any type in the Ord typeclass
orderedListProp = forAll generator test
  where
  generator = orderedList :: Gen [Float]
  test = isListOrdered

isListOrdered :: Ord a => [a] -> Bool
isListOrdered xs = snd $ foldr go (Nothing, True) xs
  where
  go _ status@(_, False)      = status
  go x (Just xPrevious, flag) = (Just x, xPrevious >= x)
  go x (Nothing, flag)        = (Just x, flag) -- first iteration

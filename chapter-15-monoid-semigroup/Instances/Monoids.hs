module Monoids where

import Control.Monad
import Data.Semigroup
import Test.QuickCheck

test :: IO ()
test = do
  quickCheck (isSemigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (isMonoidLeftIdent :: Trivial -> Bool)
  quickCheck (isMonoidRightIdent :: Trivial -> Bool)



-- tests

isSemigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
isSemigroupAssoc a b c = groupingA == groupingB
  where
  groupingA = (a <> b) <> c
  groupingB = a <> (b <> c)

isMonoidLeftIdent :: (Eq a, Monoid a) => a -> Bool
isMonoidLeftIdent a = a == mappend a mempty

isMonoidRightIdent :: (Eq a, Monoid a) => a -> Bool
isMonoidRightIdent a = a == mappend mempty a



-- 1

data Trivial = Trivial
  deriving (Show, Eq)

instance Semigroup Trivial where
  x <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = elements [Trivial]

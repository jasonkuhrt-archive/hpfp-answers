
-- Template to implement every time

-- instance Semigroup () where
--   a <> b = undefined
--
-- instance Monoid () where
--   mempty = undefined
--   mappend = (<>)
--
-- instance Arbitrary () where
--   arbitrary = undefined



module Monoids where

import Control.Monad
import Data.Semigroup
import Test.QuickCheck



test :: IO ()
test = do
  quickCheck (isSemigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (isMonoidLeftIdent :: Trivial -> Bool)
  quickCheck (isMonoidRightIdent :: Trivial -> Bool)

  quickCheck (isSemigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
  quickCheck (isMonoidLeftIdent :: Identity String -> Bool)
  quickCheck (isMonoidRightIdent :: Identity String -> Bool)

  quickCheck (isSemigroupAssoc :: Two String String -> Two String String -> Two String String -> Bool)
  quickCheck (isMonoidLeftIdent :: Two String String -> Bool)
  quickCheck (isMonoidRightIdent :: Two String String -> Bool)

  quickCheck (isSemigroupAssoc :: Conjunction -> Conjunction -> Conjunction -> Bool)
  quickCheck (isMonoidLeftIdent :: Conjunction -> Bool)
  quickCheck (isMonoidRightIdent :: Conjunction -> Bool)

  quickCheck (isSemigroupAssoc :: Disjunction -> Disjunction -> Disjunction -> Bool)
  quickCheck (isMonoidLeftIdent :: Disjunction -> Bool)
  quickCheck (isMonoidRightIdent :: Disjunction -> Bool)

  quickCheck (isSemigroupAssoc :: Or String String -> Or String String -> Or String String -> Bool)
  quickCheck (isMonoidLeftIdent :: Or String String -> Bool)
  quickCheck (isMonoidRightIdent :: Or String String -> Bool)



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
  deriving (Eq, Show)

instance Semigroup Trivial where
  x <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = elements [Trivial]



-- 2

newtype Identity a = Identity a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    pure (Identity a)



-- 3

data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    pure (Two a b)



-- 4

newtype Conjunction = Conjunction Bool
  deriving (Eq, Show)

instance Semigroup Conjunction where
  (Conjunction False) <> _ = Conjunction False
  _ <> (Conjunction False) = Conjunction False
  _ <> _                   = Conjunction True

instance Monoid Conjunction where
  mempty = Conjunction True
  mappend = (<>)

instance Arbitrary Conjunction where
  arbitrary = elements [Conjunction True, Conjunction False]



-- 5

newtype Disjunction = Disjunction Bool
  deriving (Eq, Show)

instance Semigroup Disjunction where
  t@(Disjunction True) <> _  = t
  _ <> t@(Disjunction True)  = t
  f <> _                      = f -- Choice of argument to use inconsequential.

instance Monoid Disjunction where
  mempty = Disjunction False
  mappend = (<>)

instance Arbitrary Disjunction where
  arbitrary = elements [Disjunction True, Disjunction False]



-- 6

-- TODO Monoid Append Left Identity (x == f x mempty) seems impossible?
--      According to examples type variables `a` and `b` cannot have Semigroup
--      constraints. Example also requires Semigroup Append to return last
--      `Fst _` if no `Snd _` is present, hence actually `mempty == f x mempty`!
--
--      If we required that `a` implement Semigroup then we could wiggle a
--      solution.

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  snd@(Snd _) <> _   = snd
  _ <> snd@(Snd _)   = snd
  _ <> fst           = fst

instance (Monoid a) => Monoid (Or a b) where
  mempty = Fst mempty -- ?? What can we do here to permit Left Identity ??
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]



-- 7

newtype Combine a b = Combine { unCombine :: a -> b }

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f1) <> (Combine f2) = Combine (\x -> f1 x <> f2 x)

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine $ const mempty
  mappend = (<>)



-- 8

newtype Comp a = Comp { unComp :: a -> a }

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f1) <> (Comp f2) = Comp (f1 . f2)

instance (Monoid a, Semigroup a) => Monoid (Comp a) where
  mempty = Comp $ const mempty
  mappend = (<>)



-- 10

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance (Semigroup a) => Semigroup (Mem s a) where
  (Mem f1) <> (Mem f2) = Mem f
    where
    f s = (a, b)
      where
      a = fst (f1 s) <> fst (f2 s)
      b = snd . f2 . snd . f1 $ s

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))
  mappend = (<>)

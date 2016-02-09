module Semigroups where


import Control.Monad
import Data.Semigroup
import Test.QuickCheck


test :: IO ()
test = do
  quickCheck (isSemigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (isSemigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)
  quickCheck (isSemigroupAssoc :: Two String String -> Two String String -> Two String String -> Bool)
  quickCheck (isSemigroupAssoc :: Three String String String -> Three String String String -> Three String String String -> Bool)
  quickCheck (isSemigroupAssoc :: Four String String String String -> Four String String String String -> Four String String String String -> Bool)
  quickCheck (isSemigroupAssoc :: BoolConj -> BoolConj -> BoolConj -> Bool)
  quickCheck (isSemigroupAssoc :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  quickCheck (isSemigroupAssoc :: Or String String -> Or String String -> Or String String -> Bool)
  quickCheck (isSemigroupAssoc :: Validation String String -> Validation String String -> Validation String String -> Bool)
  quickCheck (isSemigroupAssoc :: AR String String -> AR String String -> AR String String -> Bool)
  quickCheck (isSemigroupAssoc :: AB String String -> AB String String -> AB String String -> Bool)



isSemigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
isSemigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)



-- 1

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  x <> _ =  x

instance Arbitrary Trivial where
  arbitrary = pure Trivial



-- 2

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
  x <> _ = x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary



-- 3

data Two a b = Two a b deriving (Eq, Show)

instance Semigroup (Two a b) where
  (Two x _) <> (Two _ z) = Two x z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    z <- arbitrary
    return (Two x z)



-- 4

data Three a b c = Three a b c deriving (Eq, Show)

instance Semigroup (Three a b c) where
  x <> _ = x

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)



-- 5

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Semigroup (Four a b c d) where
  x <> _ = x

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)



-- 6

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]



-- 7

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _ = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj True, BoolDisj False]



-- 8

data Or a b = Fst a | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  x@(Snd _) <> _ = x
  _ <> x@(Snd _) = x
  _ <> x = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]



-- 9

newtype Combine a b = Combine { unCombine :: a -> b }

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f1) <> (Combine f2) = Combine (\x -> f1 x <> f2 x)

-- Testing this requires understanding CoArbitrary which I do not understand yet. So, the following is a manual test we can use:

f = Combine (\n -> Sum (n + 1))
g = Combine (\n -> Sum (n - 1))
-- 0 == getSum ((unCombine (f <> g)) 0)
-- 2 == getSum ((unCombine (f <> g)) 1)
-- 2 == getSum ((unCombine (g <> f)) 1)
-- 4 == getSum ((unCombine (f <> f)) 1)



-- 10

newtype Comp a =  Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
  (Comp f1) <> (Comp f2) = Comp (f2 . f1)

f2 = Combine (\n -> Sum (n + 1))
g2 = Combine (\n -> Sum (n - 1))
-- 0 == getSum ((unCombine (f2 <> g2)) 0)
-- 2 == getSum ((unCombine (f2 <> g2)) 1)
-- 2 == getSum ((unCombine (g2 <> f2)) 1)
-- 4 == getSum ((unCombine (f2 <> f2)) 1)



-- 11

data Validation a b =
    Failed a
  | OK b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  x@(OK _) <> _             = x
  _ <> x@(OK _)             = x
  (Failed x) <> (Failed z)  = Failed (x <> z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Failed a, OK b]



-- 12

-- AR stands for AccumulateRight
newtype AR a b = AR (Validation a b)
  deriving (Eq, Show)

instance (Semigroup b) => Semigroup (AR a b) where
  (AR (OK x)) <> (AR (OK z))  = AR (OK (x <> z))
  x@(AR (OK _)) <> _          = x
  _ <> x@(AR (OK _))          = x
  x <> _                      = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (AR a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [AR (Failed a), AR (OK b)]



-- 13

-- AB stands for AccumulateBoth
newtype AB a b = AB (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AB a b) where
  (AB (OK x)) <> (AB (OK z))          = AB (OK (x <> z))
  x@(AB (OK _)) <> _                  = x
  _ <> x@(AB (OK _))                  = x
  (AB (Failed x)) <> (AB (Failed z))  = AB (Failed (x <> z))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AB a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [AB (Failed a), AB (OK b)]

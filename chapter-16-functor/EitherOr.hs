module EitherOr where

import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Function

type C = Char



-- Test Runner

main :: IO ()
main = do
  quickCheck (\x -> functorIdentity (x :: EitherOr C C))
  quickCheck (functorCompose :: EitherOr C C -> Fun C C -> Fun C C -> Bool)



-- Functor Property Tests

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity a = id a == fmap id a

functorCompose :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompose a (Fun _ f) (Fun _ g) =
  fmap (g . f) a == (fmap g . fmap f) a



-- Misc

liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

-- "Surprising" (to me) feature of `liftedShow`
--
-- liftedShow Just 5 == "Just 5"
-- liftedShow Just :: Show a => a -> String
--
-- But this does not work for liftedInc:

-- Non type-variable argument in the constraint: Num (Maybe a)
-- (Use FlexibleContexts to permit this)
-- When checking that ‘it’ has the inferred type
--   it :: forall a. Num (Maybe a) => a -> Maybe a



-- Custom Either type

data EitherOr a b =
    Failure a
  | Success b
  deriving (Eq, Show)

instance Functor (EitherOr a) where
  fmap _ (Failure a) = Failure a
  fmap f (Success a) = Success (f a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (EitherOr a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Failure a, Success b]

-- Why is a Functor instance that applies the function only to Failure
-- (Either’s Left) impossible?
--
-- Becaues of two facts:
-- 1. currying proceeds left-to-right and
-- 2. EitherOr is kind (* -> * -> *) but Functor TypeClass expects (* -> *)
--
-- We can curry EitherOr to produce kind * -> * so that Functor accepts it
-- but this means preventing the Functor instance from being able to touch
-- `Failure a` because that constructor uses the type variable in the first
-- * position which we just curried.

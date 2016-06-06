module Chapter20Exercises where

import Data.Monoid



-- 1
data Constant a b = Constant a

instance Foldable (Constant a) where
  foldr _ constant _ = constant

-- 2
data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ x) = f x

-- 3
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ x) = f x

-- 4
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ x z) = f x <> f z

-- 5
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ x z o) = f x <> f z <> f o



-- Write filter for foldables --

filterF :: (Applicative f, Foldable t, Monoid (f a)) =>
           (a -> Bool) -> t a -> f a

-- This version avoids having to put type anotations
-- on the return value.
-- filterF :: (Applicative t, Foldable t, Monoid (t a)) =>
--            (a -> Bool) -> t a -> t a

filterF f = foldMap doFilter
  where
  doFilter x
    | f x       = pure x
    | otherwise = mempty


-- Recreate the standard foldable functions.
module LibraryFunctions where

import Data.Monoid



-- 1
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum
-- Or
-- sum = foldr (+) 0

-- 2
product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

-- 3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr f False -- eta reduce
  where
  f _ True      = True
  f item False  = item == x

-- 4
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f Nothing
  where
  f x Nothing      = Just x
  f x (Just minSoFar)
    | x < minSoFar = Just x
    | otherwise    = Just minSoFar

-- 5
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr f Nothing
  where
  f x Nothing      = Just x
  f x (Just minSoFar)
    | x > minSoFar = Just x
    | otherwise    = Just minSoFar

-- 6
null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True


-- 7
length' :: (Foldable t) => t a -> Int
length' = foldr (\_ n -> n + 1) 0

-- 8
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- 9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id
-- Or
-- fold' = foldr (<>) mempty

-- 10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> f x <> acc) mempty

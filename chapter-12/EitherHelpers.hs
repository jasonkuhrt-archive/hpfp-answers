module EitherHelpers where

import Data.Either


-- Reimplement core Either functions. Function names here match
-- their core equivilants save for the "'" suffix.

-- 1

lefts' :: [Either a b] -> [a]
lefts' = foldr go []
  where
  go (Left  l) ls = l : ls
  go (Right _) ls = ls



-- 2

rights' :: [Either a b] -> [b]
rights' = foldr go []
  where
  go (Left  _) rs = rs
  go (Right r) rs = r : rs



-- 3

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([], [])
  where
  go (Left  l) (ls, rs) = (,) (l:ls) rs
  go (Right r) (ls, rs) = (,) ls (r:rs)



-- 4

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right r) = Just (f r)



-- 5

either' :: (l -> x) -> (r -> x) -> Either l r -> x
either' f1 _ (Left  l) = f1 l
either' _ f2 (Right r) = f2 r



-- 6

eitherMaybe'' :: (r -> x) -> Either l r -> Maybe x
eitherMaybe'' f = either' (const Nothing) (Just . f)

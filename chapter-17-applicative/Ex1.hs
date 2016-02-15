module Ex1 where

import Control.Applicative
import Data.List

apply :: (Applicative f) => f (a -> b) -> f a -> f b
apply = (<*>)






-- 1
added :: Maybe Integer
added = fmap (+3) ((lookup (3 :: Integer)) (zip [1,2,3] [4,5,6]))



-- 2
y :: Maybe Integer
y = (lookup (3 :: Integer)) (zip [1,2,3] [4,5,6])

z :: Maybe Integer
z = (lookup (2 :: Integer)) (zip [1,2,3] [4,5,6])

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
-- Alternative Answers --
-- tupled = apply (fmap (,) y) z
-- tupled = liftA2 (,) y z
-- tupled = apply (liftA (,) y) z
-- tupled = liftA (,) y <*> z



-- 3
a :: Maybe Int
a = elemIndex (3 :: Int) [1,2,3,4,5,6,7]

b :: Maybe Int
b = elemIndex (6 :: Int) [1,2,3,4,5,6,7]

calcMax :: Int -> Int -> Int
calcMax = max

maxed :: Maybe Int
maxed = calcMax <$> a <*> b
-- Alternative Answers --
-- See #2



-- 4

xs :: [Integer]
xs = [1,2,3]

ys :: [Integer]
ys = [4,5,6]

c :: Maybe Integer
c = lookup (3 :: Integer) (zip xs ys)

d :: Maybe Integer
d = lookup (2 :: Integer) (zip xs ys)

summed :: Maybe Integer
summed = sum <$> ((,) <$> c <*> d)

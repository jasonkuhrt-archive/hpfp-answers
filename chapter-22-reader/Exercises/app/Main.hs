module Main where

import Prelude hiding (lookup)
import qualified Data.Maybe as Maybe
import qualified Control.Applicative as A



main :: IO ()
main = do

  -- Examples:
  -- print $ sequenceA [Just 3, Just 2, Just 1]
  -- print $ sequenceA [x, y]
  -- print $ sequenceA [xs, ys]
  -- print $ summed <$> ((,) <$> xs <*> ys)
  -- print $ fmap summed ((,) <$> xs <*> zs)
  -- print $ bolt 7
  -- print $ fmap bolt z
  -- print $ sequenceA [(>3), (<8), even] 7

  -- 1. fold the boolean conjunction operator over the list of results of sequA (applied to some value).

  print $ foldr (&&) False $ sequA 1

  -- 2. apply sequA to s'; you’ll need fromMaybe.

  print $ sequA . Maybe.fromMaybe 0 $ s'

  -- 3. apply bolt to ys; you’ll need fromMaybe.

  print $ bolt . Maybe.fromMaybe 0 $ ys

  -- 4. apply bolt to z'.

  print $ bolt . Maybe.fromMaybe 0 . z' $ 1



-- Data --

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

xs :: Maybe Integer
xs = lookup 3 (zip x y)

ys :: Maybe Integer
ys = lookup 6 (zip y z)

zs :: Maybe Integer
zs = lookup 4 (zip x y)

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)



-- Functions --

z' :: Integer -> Maybe Integer
z' key = lookup key (zip x z)


x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 key = (,) (z' key) (z' key)


summed :: Num n => (n,n) -> n
summed = uncurry (+)
-- summed (a,b) = a + b -- Manually


lookup :: Eq a => a -> [(a,b)] -> Maybe b
lookup _ []             = Maybe.Nothing
lookup searchingFor ((key,val):xs)
  | key == searchingFor = Maybe.Just val
  | otherwise           = lookup searchingFor xs


sequA :: Integral n => n -> [Bool]
sequA = sequenceA [(>3), (<8), even]


bolt :: Integer -> Bool
-- Compose via Functor for Functions and Applicative for Functions: compose && after >3 which results in && partially applied, embedded within a new functioning taking what >3 accepts. Applicatively Apply said result with <8 which lifts the embedded partially applied && and composes that after <8 which results in a function taking one argument that passes it to the two functions that we composed && after (>3 and <8), then with the two result values from that, pass them both to the function we composed in after.
bolt = (&&) <$> (>3) <*> (<8)
-- bolt n = (n>3) && (n<8)

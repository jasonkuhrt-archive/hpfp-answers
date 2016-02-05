module QuickChecks where

import Data.Monoid
import Test.QuickCheck



isAssociative :: (Eq a, Monoid a) => (a -> a -> a) -> a -> a -> a -> Bool
isAssociative (<|>) v x z =
  v <|> (x <|> z) == (v <|> x) <> z

isMAssociative :: (Eq a, Monoid a) => a -> a -> a -> Bool
isMAssociative v x z =
  v <> (x <> z) == (v <> x) <> z

isMLeftIdent :: (Eq a, Monoid a) => a -> Bool
isMLeftIdent x = (x <> mempty) == x

isMRightIdent :: (Eq a, Monoid a) => a -> Bool
isMRightIdent x = x == (mempty <> x)

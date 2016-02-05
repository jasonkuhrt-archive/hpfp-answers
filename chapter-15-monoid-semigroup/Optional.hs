module Optional where

import Data.Monoid



data Optional a =
    Nada
  | Only a
  deriving (Show, Eq)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada x = x
  mappend x Nada = x
  mappend (Only x) (Only z) = Only (mappend x z)



-- Ad-hoc Tests

t1 :: Num a => Optional (Sum a)
t1 = Only (Sum 1) <> Only (Sum 1)

t2 :: Num a => Optional (Sum a)
t2 = Only (Sum 1) <> Nada

t3 :: Num a => Optional (Sum a)
t3 = Nada <> Only (Sum 1)

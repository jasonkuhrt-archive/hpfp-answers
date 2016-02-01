module Main where

import Test.QuickCheck



data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)



-- 1 ~ Equal probabilitis for each:

-- instance Arbitrary Fool where
--   arbitrary = elements [Fulse, Frue]


-- 2 ~ Chance 2/3 for Fulse

instance Arbitrary Fool where
  arbitrary = frequency [(2, pure Fulse), (1, pure Frue)]

module QuickCheckCatch where

import Control.Monad
import Data.Monoid
import Test.QuickCheck
import QuickChecks



main :: IO ()
main = do
  quickCheck (isMAssociative :: Binary -> Binary -> Binary -> Bool)
-- These two tests will fail. mappend _always_ returns One. It does not care if
-- an argument is Two. So, Binary has no identity value. So, Binary is not a
-- Monoid.
  quickCheck (isMLeftIdent :: Binary -> Bool)
  quickCheck (isMRightIdent :: Binary -> Bool)



data Binary =
    One
  | Two
  deriving (Eq, Show)

instance Arbitrary Binary where
  arbitrary = elements [One, Two]

instance Monoid Binary where
  mempty = One
  mappend _ _ = One

-- This version of Optional does not require its inner values to have Monoid
-- instances. Also, it is tested with QuickCheck.

module OptionalTests where

import Control.Monad
import Optional
import QuickChecks
import Test.QuickCheck



newtype First a =
  First { getFirst :: Optional a }
  deriving (Eq, Show)

instance Monoid (First a) where
  mempty = First Nada
  mappend (First Nada) x = x
  mappend x _ = x

instance Arbitrary a => Arbitrary (First a) where
  arbitrary = frequency [
      (1, pure (First Nada)),
      (3, liftM First (liftM Only arbitrary))
    ]



main :: IO ()
main = do
  quickCheck (isMAssociative :: First String -> First String -> First String -> Bool)
  quickCheck (isMLeftIdent :: First String -> Bool)
  quickCheck (isMRightIdent :: First String -> Bool)

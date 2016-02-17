module ZipList where

import Control.Applicative hiding (ZipList)
import List hiding (main)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes



-- ZipList

newtype ZipList a = ZipList (List a)
  deriving (Eq, Show)

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend
  -- Alternative:
  -- mappend x y = mappend <$> x <*> y

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList $ fmap f xs

instance Applicative ZipList where
  pure x = ZipList $ listRepeat x
  (<*>) (ZipList fs) (ZipList xs) = ZipList $ listZipWith id fs xs



-- Test Runner

test :: IO ()
test = do
  quickBatch $ monoid $ ZipList (ListItem "" ListEnd)
  quickBatch $ functor $ ZipList $ ListItem ("","","") ListEnd
  quickBatch $ applicative $ ZipList $ ListItem ("","","") ListEnd



-- Testing

instance Eq a => EqProp (ZipList a) where
  (=-=) (ZipList xs) (ZipList zs) = eq (listTake 30 xs) (listTake 30 zs)

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = do
    x <- arbitrary
    frequency [
      (1, return (ZipList ListEnd)),
      (3, return (ZipList (ListItem x ListEnd)))
      ]

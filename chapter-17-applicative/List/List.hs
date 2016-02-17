module List where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


main :: IO ()
main = do
  quickBatch $ monoid (ListItem "" ListEnd)
  quickBatch $ functor (ListItem ("","","") ListEnd)
  quickBatch $ applicative (ListItem ("","","") ListEnd)


data List a =
    ListItem a (List a)
  | ListEnd
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = ListEnd
  mappend ListEnd zs         = zs
  mappend (ListItem x xs) zs = ListItem x (mappend xs zs)

instance Functor List where
  fmap _ (ListEnd)         = ListEnd
  fmap f (ListItem a list) = ListItem (f a) (fmap f list)

instance Applicative List where
  pure a = ListItem a ListEnd
  (<*>) _ ListEnd          = ListEnd
  (<*>) ListEnd _          = ListEnd
  (<*>) (ListItem f fs) xs = (fmap f xs) <> (fs <*> xs)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return ListEnd), (3, return (ListItem a ListEnd))]

instance CoArbitrary a => CoArbitrary (List a) where
  coarbitrary ListEnd         = variant 0
  coarbitrary (ListItem x xs) = variant 1 . coarbitrary (x, xs)


instance (Eq a) => EqProp (List a) where
  (=-=) = eq



-- Library

(<>) :: (Monoid a) => a -> a -> a
(<>) = mappend

listFold :: (a -> b -> b) -> b -> List a -> b
listFold _ seed ListEnd = seed
listFold f seed (ListItem x xs) = f x (listFold f seed xs)

listConcat :: List (List a) -> List a
listConcat = listFold (<>) ListEnd

listFlatMap :: (a -> List b) -> List a -> List b
listFlatMap _ ListEnd = ListEnd
listFlatMap f xs      = listConcat (fmap f xs)

listTake :: Int -> List a -> List a
listTake _ ListEnd = ListEnd
listTake n (ListItem x xs)
  | n <= 0    = ListEnd
  | otherwise = ListItem x (listTake (n - 1) xs)

listRepeat :: a -> List a
listRepeat x = ListItem x (listRepeat x)

listZipWith :: (a -> b -> c) -> List a -> List b -> List c
listZipWith _ ListEnd _ = ListEnd
listZipWith _ _ ListEnd = ListEnd
listZipWith f (ListItem x xs) (ListItem z zs) =
  ListItem (f x z) (listZipWith f xs zs)

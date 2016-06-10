-- Allow explicit type signatures inside instances (typeclass implementations).
{-# LANGUAGE InstanceSigs #-}

module ReadingComprehension where



-- 1 - Write liftA2

liftA2 :: Applicative a => (v -> x -> z) -> a v -> a x -> a z
liftA2 f av ax = f <$> av <*> ax



-- 2 Write asks

newtype Reader a b = Reader { runReader :: a -> b }

asks :: r -> a -> Reader r a
asks _ a = Reader (const a)



-- 3 Write implementation of Applicative Typeclass for Reader Type

instance Functor (Reader a) where
  fmap g (Reader f) = Reader (g . f)

instance Applicative (Reader r) where
  pure :: b -> Reader r b
  pure b = Reader (const b)

  -- (<*>) :: Applicative a => a (v -> x) -> a v -> a x
  -- (<*>) :: (-> r) (v -> x) -> (-> r) v -> (-> r) x
  -- (<*>) :: (r -> v -> x) -> (r -> v) -> (r -> x)
  -- So...
  -- Remember that `Reader r` is the *structure*.
  (<*>) :: Reader r (v -> x) -> Reader r v -> Reader r x
  (<*>) (Reader g) (Reader f) = Reader (\r -> g r (f r))



-- 4 Rewrite the Dog/Person example using Reader
-- TODO Use Reader as per the goal of this exercise question.

newtype DogName = DogName String deriving (Show, Eq)
newtype HumanName = HumanName String deriving (Show, Eq)
newtype Address = Address String deriving (Show, Eq)

data Person =
  Person {
    humanName :: HumanName,
    address :: Address,
    dogName :: DogName
  } deriving (Show, Eq)

data Dog =
  Dog {
    dogsName :: DogName,
    dogsAddress :: Address
  } deriving (Show, Eq)

person :: Person
person = Person
  (HumanName "Jason") (Address "5624 Hutchison") (DogName "Woofie")

getDog :: Person -> Dog
getDog = Dog <$> dogName <*> address

test4a :: Dog
test4a = getDog person

getDogReader :: Reader Person Dog
getDogReader = Dog <$> Reader dogName <*> Reader address

test4b :: Dog
test4b = runReader getDogReader person

test4c :: Bool
test4c = test4a == test4b

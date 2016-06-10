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

newtype DogName = DogName String deriving (Show)
newtype HumanName = HumanName String deriving (Show)
newtype Address = Address String deriving (Show)

data Person =
  Person {
    humanName :: HumanName,
    address :: Address,
    dogName :: DogName
  } deriving (Show)

data Dog =
  Dog {
    dogsName :: DogName,
    dogsAddress :: Address
  } deriving (Show)

getDog :: Person -> Dog
getDog = Dog <$> dogName <*> address

test4 :: Dog
test4 = getDog person
  where
  person = Person
    (HumanName "Jason")
    (Address "5624 Hutchison")
    (DogName "Woofie")

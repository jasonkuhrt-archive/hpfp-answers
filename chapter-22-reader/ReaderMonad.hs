{-# LANGUAGE InstanceSigs #-} -- Encourage more learning

module ReaderMonad where

{-# ANN module "HLint: ignore Avoid lambda" #-}



-- 1 Implement the Reader Monad

newtype Reader a b =
  Reader { runReader :: a -> b }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap g (Reader f) = Reader (\r -> g (f r))

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure b = Reader (const b)
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader g) (Reader f) = Reader (\r -> g r (f r))

instance Monad (Reader r) where
  return :: a -> Reader r a
  return a = Reader (const a)
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader f) g = Reader (\r -> runReader (g (f r)) r)



-- 2 Rewrite getDog using Reader Monad Instance

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

dog :: Dog
dog = Dog (DogName "Woofie") (Address "5624 Hutchison")


getDog :: Person -> Dog
getDog = Dog <$> dogName <*> address

getDogReader :: Reader Person Dog
getDogReader = Dog <$> Reader dogName <*> Reader address

getDogDo :: Reader Person Dog
getDogDo = do
  n <- Reader dogName
  a <- Reader address
  return (Dog n a)

getDogMonad :: Reader Person Dog
getDogMonad =
  Reader dogName >>= (\n ->
    Reader address >>= (\a ->
      return (Dog n a)
    )
  )

test2 :: Bool
test2 = dog == runReader getDogMonad person &&
        dog == runReader getDogDo person

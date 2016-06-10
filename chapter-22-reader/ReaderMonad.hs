{-# LANGUAGE InstanceSigs #-} -- Encourage more learning

module ReaderMonad where

{-# ANN module "HLint: ignore Avoid lambda" #-}



-- 1 Implement the Reader Monad

newtype Reader a b =
  Reader { runReader :: a -> b }

instance Functor (Reader a) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap g (Reader f) = Reader (\r -> g (f r))

instance Applicative (Reader a) where
  pure :: a -> Reader r a
  pure b = Reader (const b)
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) (Reader g) (Reader f) = Reader (\r -> g r (f r))

instance Monad (Reader a) where
  return :: a -> Reader r a
  return a = Reader (const a)
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader f) g = Reader (\r -> runReader (g (f r)) r)

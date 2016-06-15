{-# LANGUAGE InstanceSigs #-}

module ChapterExercises where

import qualified Control.Applicative as A



-- 1 Write Traversable instances

-- A

newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
instance Foldable Identity where
  foldr f seed (Identity x) = f x seed
instance Traversable Identity where
  traverse :: Applicative a => (x -> a z) -> Identity x -> a (Identity z)
  traverse f (Identity x) = fmap Identity (f x)

-- B

newtype Constant a b = Constant { getConstant :: a }

instance Functor (Constant c) where
  fmap :: (x -> z) -> Constant c x -> Constant c z
  fmap _ (Constant c) = Constant c -- Reconstruct to permit type change.
instance Foldable (Constant c) where
  foldr :: (x -> z -> z) -> z -> Constant c x -> z
  foldr _ seed _ = seed
instance Traversable (Constant c) where
  -- While the `f` param is not needed for execution at runtime it is still needed for its type which provides the means unto which we know which `pure` implementation to dispatch to. How? Well, `traverse` results in `a (t z)` and `f` type is `x -> a z`. Take note of the `a`. The result is contained in the same `a` that the result of `f` is. Now enter `pure` of type `Applicative a => x -> a x` which injects something into a `a` container... When this code is evaluated the specific `pure` to dispatch to is inferred by the `a` that the runtime-unused `f` results in. For example compare `traverse (:[]) (Constant 1) == [Constant 1]`, to `traverse Just (Constant 1) == Just (Constant 1)`. In both of these the given `f` appears to have the affect of wrapping the Constant but what is actually happen is that its result type is tipping off `pure` under the hood how to behave (which instance to dispatch too)!
  traverse :: Applicative a => (x -> a z) -> Constant c x -> a (Constant c z)
  traverse _ (Constant c) = pure (Constant c)

-- C

data Optional a = Something a | Nil

instance Functor Optional where
  fmap :: (x -> z) -> Optional x -> Optional z
  fmap _ Nil           = Nil
  fmap f (Something x) = Something (f x)
instance Foldable Optional where
  foldMap :: Monoid m => (x -> m) -> Optional x -> m
  foldMap _ Nil           = mempty
  foldMap f (Something x) = f x
instance Traversable Optional where
  traverse :: Applicative a => (x -> a z) -> Optional x -> a (Optional z)
  traverse _ Nil = pure Nil
  traverse f (Something x) = fmap Something (f x)

-- D

data List a = Item a (List a) | Empty deriving (Show)

instance Functor List where
  fmap :: (x -> z) -> List x -> List z
  fmap _ Empty         = Empty
  fmap f (Item x rest) = Item (f x) (fmap f rest)
instance Foldable List where
  foldMap :: Monoid m => (x -> m) -> List x -> m
  foldMap _ Empty         = mempty
  foldMap f (Item x rest) = mappend (f x) (foldMap f rest)
instance Traversable List where
  -- Although <*> is infixl the recursion nests expressions resulting in the rightward-like evaluation. E.g. *not* `fmap Item (f x) <*> fmap Item (f x) <*> pure Empty` but rather `fmap Item (f x) <*> (fmap Item (f x) <*> (pure Empty))`. If you write traverse *not* using infix notation I think this expansion becomes easier to imagine/simulate mentally.
  traverse :: Applicative a => (x -> a z) -> List x -> a (List z)
  traverse _ Empty = pure Empty
  traverse f (Item x rest) = (<*>) (fmap Item (f x)) (traverse f rest)

-- E

data Three a b c = Three a b c deriving (Show)

instance Functor (Three a b) where
  fmap :: (x -> z) -> Three a b x -> Three a b z
  fmap f (Three a b x) = Three a b (f x)
instance Foldable (Three a b) where
  foldMap :: Monoid m => (x -> m) -> Three a b x -> m
  foldMap f (Three _ _ x) = f x
instance Traversable (Three a b) where
  traverse :: Applicative c => (x -> c z) -> Three a b x -> c (Three a b z)
  traverse f (Three a b x) = fmap (Three a b) (f x)

-- F

data Three' a b = Three' a b b deriving (Show)

instance Functor (Three' a) where
  fmap :: (x -> z) -> Three' a x -> Three' a z
  fmap f (Three' a x z) = Three' a (f x) (f z)
instance Foldable (Three' a) where
  foldMap :: Monoid m => (x -> m) -> Three' a x -> m
  foldMap f (Three' _ x z) = mappend (f x) (f z)
instance Traversable (Three' a) where
  traverse :: Applicative c => (x -> c z) -> Three' a x -> c (Three' a z)
  traverse f (Three' a x z) = A.liftA2 (Three' a) (f x) (f z)
  -- AKA
  -- traverse f (Three' a x z) = Three' a <$> f x <*> f z
  -- traverse f (Three' a x z) = (<*>) (fmap (Three' a) (f x)) (f z)

  -- G

data S n a = S (n a) a deriving (Show)

instance Functor n => Functor (S n) where
  fmap :: (x -> z) -> S n x -> S n z
  fmap f (S nx x) = S (fmap f nx) (f x)
instance Foldable (S n) where
  foldMap :: Monoid m => (x -> m) -> S n x -> m
  foldMap f (S _ x) = f x
instance Traversable n => Traversable (S n) where
  traverse :: Applicative a => (x -> a z) -> S n x -> a (S n z)
  -- fmap (x -> a z) (n x) :: n (a z) |> sequenceA :: a (n z)
  traverse f (S nx x) = S <$> (sequenceA . fmap f $ nx) <*> f x



-- 2 Implement typeclasses for type Tree

data Tree a =
  Node (Tree a) a (Tree a) |
  Leaf a                   |
  End
  deriving (Show, Eq)

instance Functor Tree where
  fmap :: (x -> z) -> Tree x -> Tree z
  fmap _ End =
    End
  fmap f (Leaf x) =
    Leaf (f x)
  fmap f (Node treeLeft x treeRight) =
    Node (fmap f treeLeft) (f x) (fmap f treeRight)
instance Foldable Tree where
  foldMap :: Monoid m => (x -> m) -> Tree x -> m
  foldMap _ End =
    mempty
  foldMap f (Leaf x) =
    f x
  foldMap f (Node treeLeft x treeRight) =
    mappend (mappend (foldMap f treeLeft) (f x)) (foldMap f treeRight)
instance Traversable Tree where
  traverse :: Applicative a => (x -> a z) -> Tree x -> a (Tree z)
  traverse _ End =
    pure End
  traverse f (Leaf x) =
    fmap Leaf (f x)
  traverse f (Node treeLeft x treeRight) =
    Node <$> traverse f treeLeft
         <*> f x
         <*> traverse f treeRight

module InstancesMaybe where

-- Are Functor instances possible?

import GHC.Arr



-- 1
data B = F | T

-- Answer: No, because the data type kind is `*` rather than the necessary higher-order `* -> *`.



-- 2
data BoolAndSomethingElse a = F2 a | T2 a

-- Answer: Yes, because the data type kind is * -> *
instance Functor BoolAndSomethingElse where
  fmap f (F2 a) = F2 (f a)
  fmap f (T2 a) = T2 (f a)



-- 3
data BoolAndMaybeSomethingElse a = F3 | T3 a

-- Answer: Yes, because the data type kind is * -> *
instance Functor BoolAndMaybeSomethingElse where
  fmap f (T3 a) = T3 (f a)
  fmap _ F3 = F3



-- 4
newtype Mu f = InF { outF :: f (Mu f) }

-- Answer: No, because the kind is (* -> *) -> * but Functor expects * -> *



-- 5
data D = D (Array Word Word) Int Int

-- Answer: No, because the kind is *, but Functor expects * -> *

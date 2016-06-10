
module WarmingUp where

import Data.Char

{-# ANN module "HLint: ignore Use String" #-}



-- Functions to compose later --

toUppercase :: [Char] -> [Char]
toUppercase = fmap toUpper

invert :: [a] -> [a]
invert = reverse



-- Compose --

composed :: [Char] -> [Char]
composed = toUppercase . invert

composed2 :: [Char] -> [Char]
composed2 = fmap toUppercase invert

isWorking :: Bool
isWorking = composed "abc" == composed2 "abc"



-- Tuple --

-- First: Tuple (constructor) is lifted over invert which means passing the information after the leading `a ->` (which is a) to Tuple (constructor); And what this is also known as is composition becuase we are saying that a value should pass through `invert` and then into Tuple (constructor).
-- Second: Tuple (constructor) takes two arguments. Feeding one value means getting back another function which takes one argument. Therefore feeding our composition (`invert` into `Tuple`) returns a function expecting one argument. From the type perspective this looks like: a -> b -> (a,b).
-- Third: a -> b -> (a,b) is an applicative because inside the leading structure of `a ->` we have a function `b -> (a,b)`. If we try to applicatively compose this via <*> with `toUppercase` we are in effect lifting, firstly, the embedded `b -> (a,b)`, and secondly `[Char]` from `toUppercase` (type of ([Char] -> [Char])) which, like in step 1 means to feed `[Char]` to `b -> (a,b)`.
-- Fourth: Thinking of step 2/3 together we see the following computation emerge: invert string, pass it to Tuple, toUppercase string, pass it to Tuple. NOTE it appears that the implementation choice was to make it so that string need only be passed once and have it sent to both functions at once but another implementation seems possible wherein there are two string params each feeding one of the aforementioned pipelines?
applicativeTuple :: [Char] -> ([Char], [Char])
applicativeTuple = (,) <$> invert <*> toUppercase

doTuple :: [Char] -> ([Char], [Char])
doTuple = do
  a <- toUppercase
  b <- invert
  return (a,b)

monadTuple :: [Char] -> ([Char], [Char])
monadTuple =
  toUppercase >>= (\a ->
    invert >>= (\b ->
      return (a, b)
    )
  )


-- newtype Function a b = Function ((->) a b)
--
-- apply :: Function a b -> a -> b
-- apply (Function f) = f
--
-- compose :: Function b c -> Function a b -> Function a c
-- compose (Function g) (Function f) = Function (g . f)
--
-- instance Functor (Function a) where
--   fmap g (Function f) = Function (g . f)
--   -- fmap = compose
--
-- instance Applicative (Function a) where
--     pure = undefined
--     (<*>) (Function f) (Function g) = Function $ \x z -> f x (g z)

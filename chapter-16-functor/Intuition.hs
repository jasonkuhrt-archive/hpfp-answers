module Intutition where


replaceWithA :: a -> Char
replaceWithA = const 'A'

a :: Maybe Char
a = fmap replaceWithA (Just (10 :: Integer))
-- Just 'A'

b :: Maybe Char
b = fmap replaceWithA Nothing
-- Nothing

c :: String
c = fmap replaceWithA ([1,2,3,4] :: [Integer])
-- ['A','A','A','A'] AKA "AAAA"

d :: String
d = fmap replaceWithA ""
-- [] AKA ""



e :: (Integer, Char)
e = fmap replaceWithA (5, "foobar")
-- (5, 'A')



f :: (Integer, String)
f = fmap (fmap replaceWithA) (5, "foobar")
f2 :: (Integer, String)
f2 = (fmap . fmap) replaceWithA (5, "foobar")
-- (5, "AAAAAA")



-- NOTE: Functor of functions is composition! (seemingly)
g :: Num a => a -> a
g = fmap (+1) negate



h :: Num a => a
h = g 10
-- -9



append :: Monoid a => a -> a -> a
append = flip mappend

i :: String
i = (fmap (append "!") (fmap (append "!") (append "!"))) "Argh"
-- "Argh!!!"


j :: [Maybe String]
j = (fmap . fmap . fmap) replaceWithA [Just "Jason", Nothing, Just "Kuhrt"]
-- [Just "AAAAA", Nothing, Just "AAAAA"]



data1 :: [Maybe [[Char]]]
data1 = [Just ["Ha", "Ha"], Nothing, Just []]



k :: [Maybe Char]
k = (fmap . fmap) replaceWithA data1
-- [Just 'A', Nothing, Just 'A']



l :: [Maybe [Char]]
l = (fmap . fmap . fmap) replaceWithA data1
-- [Just "AA", Nothing, Just ""]



m :: [Maybe [[Char]]]
m = (fmap . fmap . fmap . fmap) replaceWithA data1
-- [Just ["AA", "AA"], Nothing, Just []]






pl :: String -> IO ()
pl = putStr

p :: Show a => a -> IO ()
p = print

data2 :: [Maybe [Char]]
data2 = [Just "Ha", Nothing, Just ""]

replaceWithA' :: [Maybe [Char]] -> Char
replaceWithA' = replaceWithA

replaceWithALifted :: Functor f => f a -> f Char
replaceWithALifted = fmap replaceWithA

replaceWithALifted' :: [Maybe [Char]] -> [Char]
replaceWithALifted' = fmap replaceWithA

replaceWithALifted2 :: (Functor f1, Functor f2) => f1 (f2 a) -> f1 (f2 Char)
replaceWithALifted2 = (fmap . fmap) replaceWithA

replaceWithALifted2' :: [Maybe [Char]] -> [Maybe Char]
replaceWithALifted2' = (fmap . fmap) replaceWithA

replaceWithALifted3 :: (Functor f1, Functor f2, Functor f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 Char))
replaceWithALifted3 = (fmap . fmap . fmap) replaceWithA

replaceWithALifted3' :: [Maybe [Char]] -> [Maybe [Char]]
replaceWithALifted3' = (fmap . fmap . fmap) replaceWithA

demo :: IO ()
demo = do
  pl "replaceWithA data2:  "
  p (replaceWithA data2)

  pl "replaceWithA' data2:  "
  p (replaceWithA' data2)

  pl "replaceWithALifted data2:  "
  p (replaceWithALifted data2)

  pl "replaceWithALifted' data2:  "
  p (replaceWithALifted' data2)

  pl "replaceWithALifted2 data2:  "
  p (replaceWithALifted2 data2)

  pl "replaceWithALifted2' data2:  "
  p (replaceWithALifted2' data2)

  pl "replaceWithALifted3 data2:  "
  p (replaceWithALifted3 data2)

  pl "replaceWithALifted3' data2:  "
  p (replaceWithALifted3' data2)

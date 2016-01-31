module QuickCheckTests where

import Test.QuickCheck
import Data.List

cons = (:)
join = (++)
mul = (*)
eq = (==)
neq = (/=)



main :: IO ()
main = do
  quickCheck halfAddHalfIdentProp
  quickCheck halfAddHalfIdentityProp
  quickCheck sortOrdersListProp
  quickCheck plusAssociativeProp
  quickCheck plusCommutativeProp
  quickCheck multiplyAssociativeProp
  quickCheck multiplyCommutativeProp
  quickCheck quotRemLawProp
  quickCheck divModLawProp
  -- quickCheck powAssociativeProp -- False property
  -- quickCheck powCommutativeProp -- False property
  quickCheck reverseTwiceIsIdProp
  quickCheck dollarAppliesProp
  quickCheck dollarRightAssociativeProp
  -- quickCheck foldrConsEqJoinProp -- False property
  quickCheck foldrJoinEqConcatProp
  -- quickCheck takeCountEqLengthProp -- False property
  quickCheck showReadIdentProp



-- 1 -- half

halfAddHalfIdentProp = forAll generator test
  where
  generator = arbitrary :: Gen Float
  test n = (eq n . mul 2 . half) n

halfAddHalfIdentityProp = forAll generator test
  where
  generator = arbitrary :: Gen Float
  test n = n == (half n + half n)

half :: Float -> Float
half n = n / 2



-- 2 -- sort

sortOrdersListProp = forAll generator test
  where
  generator = arbitrary :: Gen [Float]
  test = isListOrdered . sort

isListOrdered :: Ord a => [a] -> Bool
isListOrdered xs = snd $ foldr go (Nothing, True) xs
  where
  go _ status@(_, False)      = status
  go x (Just xPrevious, flag) = (Just x, xPrevious >= x)
  go x (Nothing, flag)        = (Just x, flag) -- first iteration



-- 3 -- plus

plusAssociativeProp = forAll generator test
  where
  generator = arbitrary :: Gen (Integer, Integer, Integer)
  test (x, y, z) =
    (==) (x + (y + z)) ((x + y) + z)

plusCommutativeProp = forAll generator test
  where
  generator = arbitrary :: Gen (Float, Float)
  test (x, y) =
    (==) (x + y) (y + x)



-- 4 -- multiply

multiplyAssociativeProp = forAll generator test
  where
  generator = arbitrary :: Gen (Integer, Integer, Integer)
  test (x, y, z) =
    (==) (mul x (mul y z)) (mul (mul x y) z)

multiplyCommutativeProp = forAll generator test
  where
  generator = arbitrary :: Gen (Float, Float)
  test (x, y) =
    (==) (mul x y) (mul y x)



-- 5 -- quot/rem & div/mod laws

quotRemLawProp = forAll generator test
  where
  generator = suchThat (arbitrary :: Gen (Integer, Integer)) nonZeroDivisor
  test (x, y) = x == quot x y * y + rem x y

divModLawProp = forAll generator test
  where
  generator = suchThat (arbitrary :: Gen (Integer, Integer)) nonZeroDivisor
  test (x, y) = x == mul (div x y) y + mod x y

nonZeroDivisor :: (Integer, Integer) -> Bool
nonZeroDivisor (_, denom) = neq denom 0



-- 6 -- Is `^` associative? commutative?

-- No, it is not!
powAssociativeProp = forAll generator test
  where
  generator = suchThat (arbitrary :: Gen (Integer, Integer, Integer)) nonNegativeExpos
  test (x, y, z) = (x ^ (y ^ z)) == ((x ^ y) ^ z)


-- No, it is not!
powCommutativeProp = forAll generator test
  where
  generator = suchThat (arbitrary :: Gen (Integer, Integer)) nonNegativeExpo
  test (x, y) = (x ^ y) /= (y ^ x)


nonNegativeExpos :: (Integer, Integer, Integer) -> Bool
nonNegativeExpos (_, y, z) = y >= 0 && z >= 0

nonNegativeExpo :: (Integer, Integer) -> Bool
nonNegativeExpo (x, y) = x >= 0 && y >= 0



-- 7 -- reverse . reverse == id

reverseTwiceIsIdProp = forAll generator test
  where
  generator = arbitrary :: Gen [String]
  test string = (reverse . reverse $ string) == id string



-- 8 -- Property tests for $

dollarAppliesProp = forAll generator test
  where
  generator = arbitrary :: Gen Integer
  test n = id $ n == id n

dollarRightAssociativeProp = forAll generator test
  where
  generator = arbitrary :: Gen [Integer]
  test xs = ((fmap $ mul $ neg1) $ xs) == ((fmap (mul neg1)) xs)
  neg1 = (-1)



-- 9a ~ Are these equal? foldr (:) == (++)
--
--      No! Its close, but the order does not match up:
--      (++) ["a"] ["b"]      == ["a","b"]
--      foldr (:) ["a"] ["b"] == ["b","a"]
--             ^  ^     ^
--                acc   [x]
--             x:acc
--
--      In the property test below there are two commented out
--      variants that _would_ be equal.


foldrConsEqJoinProp = forAll generator test
  where
  generator = arbitrary :: Gen ([String], [String])
  test (xs,zs) = foldr cons xs zs == join xs zs
  -- test (xs,zs) = foldr cons xs zs == (flip join) xs zs
  -- test (xs,zs) = (flip $ foldr cons) xs zs == join xs zs



-- 9b ~ Are these equal? foldr (++) [] == concat
--
--      Yes! Consider:
--
--      concat [[1],[2]]        == [1,2]
--      foldr join [] [[1],[2]] == [1,2]
--            ^    ^       ^
--                 acc     x
--            x ++ acc

foldrJoinEqConcatProp = forAll generator test
  where
  generator = arbitrary :: Gen [[Integer]]
  test xs = foldr join [] xs == concat xs


-- 10 ~ Is this true? f n xs = length (take n xs) == n
--      No! If xs length is less than n to begin with then this
--      property does not hold. Consider:
--
--      False == (length (take 1 []) == 1)

takeCountEqLengthProp = forAll generator test
  where
  generator = suchThat (arbitrary :: Gen (Int, [Integer])) nonZeroCount
  test (n, xs) = length (take n xs) == n
  nonZeroCount (n, _) = n >= 0



-- 11 ~ Test this property: f x = (read (show x)) == x

showReadIdentProp = forAll generator test
  where
  generator = arbitrary :: Gen String
  test x = (read (show x)) == x

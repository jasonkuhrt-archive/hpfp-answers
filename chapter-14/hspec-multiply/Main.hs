module Main where

import Test.Hspec


main :: IO ()
main = hspec $ do

  describe "Multiply basics" $ do

    it "mul 5 5 is 25" $
      shouldBe 25 (mul 5 5 :: Integer)

    it "mul 5 -5 is -25" $
      shouldBe (-25) (mul 5 (-5) :: Integer)

    it "mul -5 5 is -25" $
      shouldBe (-25) (mul (-5) 5 :: Integer)

    it "mul -5 -5 is 25" $
      shouldBe 25 (mul (-5) (-5) :: Integer)

  describe "with 0" $ do

    it "mul 0 0 is 0" $
      shouldBe 0 (mul 0 0 :: Integer)

    it "mul 0 5 is 0" $
      shouldBe 0 (mul 0 5 :: Integer)

    it "mul 5 0 is 0" $
      shouldBe 0 (mul 5 0 :: Integer)

    it "mul 0 -5 is 0" $
      shouldBe 0 (mul 0 (-5) :: Integer)

    it "mul -5 0 is 0" $
      shouldBe 0 (mul (-5) 0 :: Integer)

  describe "with 1" $ do

    it "mul 1 1 is 1" $
      shouldBe 1 (mul 1 1 :: Integer)

    it "mul 1 -5 is -5" $
      shouldBe (-5) (mul 1 (-5) :: Integer)

    it "mul -5 1 is -5" $
      shouldBe (-5) (mul (-5) 1 :: Integer)

    it "mul -1 5 is -5" $
      shouldBe (-5) (mul (-1) 5 :: Integer)

    it "mul 5 -1 is -5" $
      shouldBe (-5) (mul 5 (-1) :: Integer)



mul :: (Num a, Eq a, Ord a) => a -> a -> a
mul _ 0 = 0
mul 0 _ = 0
mul a b
  | b < 0     = mul a (b + 1) - a
  | otherwise = a + mul a (b - 1)

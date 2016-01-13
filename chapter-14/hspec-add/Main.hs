module Main where

import Test.Hspec



divideBy :: Integral a => a -> a -> (a, a)
divideBy num denom = go num denom 0
  where go n d count
          | n < d     = (count, n)
          | otherwise = go (n - d) d (count + 1)



main :: IO ()
main = hspec $ do



  describe "Addition" $ do

    it "1 + 1 is greater than 1" $
      shouldBe True ((1 + 1 :: Integer) > 1)

    it "2 + 2 is equal to 4" $
      shouldBe 4 ((2 + 2) :: Integer)



  describe  "divideBy" $ do

    it "15 divided by 3 is 5" $
      shouldBe (5, 0) (divideBy 15 3 :: (Integer, Integer))

    it "22 divided by 5 is 4 remainder 2" $
      shouldBe (4, 2) (divideBy 22 5 :: (Integer, Integer))

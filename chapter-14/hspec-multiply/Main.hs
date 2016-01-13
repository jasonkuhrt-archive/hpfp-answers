module Main where



mul :: (Num a, Eq a, Ord a) => a -> a -> a
mul _ 0 = 0
mul 0 _ = 0
mul a b
  | b < 0     = mul a (b + 1) - a
  | otherwise = a + mul a (b - 1)

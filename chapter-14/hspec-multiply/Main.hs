module Main where



mul :: Integer -> Integer -> Integer
mul _ 0 = 0
mul 0 _ = 0
mul a b
  | b < 0     = mul a (b + 1) - a
  | otherwise = a + mul a (b - 1)

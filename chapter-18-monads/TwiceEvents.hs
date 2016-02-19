module TwiceEvens where



twiceEvens :: [Integer] -> [Integer]
twiceEvens xs = do
  x <- xs
  if even x then [x*x, x*x] else []

-- Alternatives:

-- twiceEvens xs = xs >>= (\x -> if even x then [x*x, x*x] else [])

-- twiceEvens = (>>= doTwiceEvens)
--   where
--   doTwiceEvens x = if   even x
--                    then [x*x, x*x]
--                    else []

-- Using branch instead of if-else:

-- branch :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
-- branch p f1 f2 x = if p x then f1 x else f2 x
-- twiceEvens xs = xs >>= branch even (\x -> [x*x, x*x]) (\x -> [])

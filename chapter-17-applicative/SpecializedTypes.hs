module Exercises where


-- 1
-- []
-- pure :: a -> [a]
-- <*> :: [a -> b] -> [a] -> [b]
-- Example: pure (+1) <*> [1]



-- 2
-- IO
-- pure :: a -> IO a
-- <*> :: IO (a -> b) -> IO a -> IO b
-- Example: pure (mappend "My name is ") <*> getLine

-- 3
-- (,) a
-- pure :: b -> (a, b)
-- <*> :: (a, (b -> c)) -> (a, c) -> (a, b)
-- Example: (["programming"], (*2)) <*> (["haskell"], 1)

-- 4
-- (->) a
-- pure :: b -> (a -> b)
-- <*> :: a -> (b -> c) -> (a -> b) -> (a -> c)
-- Example: (+) <*> (*1) $ 5

-- (<*>) g f = \x -> g x (f x)

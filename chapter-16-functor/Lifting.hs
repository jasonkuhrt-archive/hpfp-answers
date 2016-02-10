module Lifting where



-- 1
a :: [Int]
a = fmap (+1) $ read "[1]"

-- 2
b :: Maybe [String]
b = ((fmap . fmap) (++ "lol")) (Just ["Hi,", "Hello"])

-- 3
c :: Num a => a -> a
c = fmap (*2) (\x -> x - 2)

-- 4
d :: (Show a, Enum a, Num a) => a -> String
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- 5
e :: IO Integer
e = fmap (*3) ioNum
  where
  ioNum = fmap (read . ("123" ++) . show) ioOne
  ioOne = readIO "1" :: IO Integer

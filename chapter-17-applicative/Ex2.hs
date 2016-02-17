module Ex2 where


-- 1
a :: Maybe String
a = const <$> Just "Hello" <*> pure "World"

-- 2
b :: (Num a, Num b, Num c) => Maybe (a, b, String, [c])
b = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tiredness" <*> pure [1,2,3]

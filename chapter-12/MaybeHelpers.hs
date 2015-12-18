module MaybeHelpers where



-- 1

isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False



-- 2

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee fallback _ Nothing = fallback
mayybee _ f (Just a)       = f a



-- 3

fromMaybe :: a -> Maybe a -> a
fromMaybe fallback Nothing = fallback
fromMaybe _ (Just a) = a



-- 4

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (a:_) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]



-- 5

catMaybes :: [Maybe a] -> [a]
catMaybes = map fromJust . filter isJust
  where
    fromJust :: Maybe a -> a
    fromJust (Just a) = a



-- 6

-- If any item in the given list is Nothing
-- then return Nothing.

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr go (Just [])
  where
  go :: Maybe a -> Maybe [a] -> Maybe [a]
  go (Just x) (Just xs) = Just ((++) xs [x])
  go Nothing _          = Nothing
  go _ Nothing          = Nothing

  -- Alternative Implementation

  -- go :: [a] -> [Maybe a] -> Maybe [a]
  -- go xs []           = Just xs
  -- go xs (Just x:ms)  = go ms (xs ++ [x])
  -- go xs (Nothing:ms) = Nothing

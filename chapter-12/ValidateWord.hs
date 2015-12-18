
module ValidateWord where

import StringProcessing



newtype MyWord =
  MyWord String
  deriving (Eq, Show)



-- A smart constructor for MyWord data. Validate that the
-- input looks like a word.

makeWord :: String -> Maybe MyWord
makeWord string = if isValidWord string
                  then Just (MyWord string)
                  else Nothing



-- Validate that input looks like a real word. Valid words should have
-- equal or fewer vowels than consonants.

isValidWord :: String -> Bool
isValidWord = go 0
  where
  go :: Integer -> String -> Bool
  go count ""     = count > -1
  go count (x:xs) = if isVowel x
                    then go (count - 1) xs
                    else go (count + 1) xs


module StringProcessing where


-- 1

-- Replace instances of the word "the" with "a".

unthe :: String -> String
unthe = unwords . map replace . words
  where
  replace :: String -> String
  replace "the" = "a"
  replace s     = s



-- 2

-- Count the number of "the" words that are followed by a word which starts with a vowel.

-- > countTheBeforeVowel "the action is the reason for the outage"
-- 2 --                   ^                            ^

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = count . words
  where

  count :: [String] -> Integer
  count xxs
    | length xxs < 2  = 0
    | isMatch xxs     = (+) 1 (count (tail xxs))
    | otherwise       = count (tail xxs)

  isMatch :: [String] -> Bool
  isMatch ("the":(letter:_):_)  = isVowel letter
  isMatch _                     = False

vowels :: String
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel = (`elem` vowels)



-- 3

-- Count the number of vowels in a word.

countVowels :: String -> Integer
countVowels = foldr count 0
  where
  count x acc
    | isVowel x = acc + 1
    | otherwise = acc

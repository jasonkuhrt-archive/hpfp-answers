module Morse (
  Morse,
  charToMorse,
  morseToChar,
  stringToMorse,
  charMorses,
  morseChars
) where

import qualified Data.Map as M

type Morse = String

charMorses :: M.Map Char Morse
charMorses = M.fromList [
      ('a', ".-")
    , ('b', "-...")
    , ('c', "-.-.")
    , ('d', "-..")
    , ('e', ".")
    , ('f', "..-.")
    , ('g', "--.")
    , ('h', "....")
    , ('i', "..")
    , ('j', ".---")
    , ('k', "-.-")
    , ('l', ".-..")
    , ('m', "--")
    , ('n', "-.")
    , ('o', "---")
    , ('p', ".--.")
    , ('q', "--.-")
    , ('r', ".-.")
    , ('s', "...")
    , ('t', "-")
    , ('u', "..-")
    , ('v', "...-")
    , ('w', ".--")
    , ('x', "-..-")
    , ('y', "-.--")
    , ('z', "--..")
    , ('1', ".----")
    , ('2', "..---")
    , ('3', "...--")
    , ('4', "....-")
    , ('5', ".....")
    , ('6', "-....")
    , ('7', "--...")
    , ('8', "---..")
    , ('9', "----.")
    , ('0', "-----")
  ]



morseChars :: M.Map Morse Char
morseChars = M.foldWithKey (flip M.insert) M.empty charMorses

charToMorse :: Char -> Maybe Morse
charToMorse char = M.lookup char charMorses

morseToChar :: Morse -> Maybe Char
morseToChar morse = M.lookup morse morseChars

stringToMorse :: String -> Maybe [Morse]
stringToMorse string = sequence $ fmap charToMorse string

-- morseToChar = undefined

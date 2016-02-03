module Vigenere where

import Data.Maybe
import Data.List
import Data.Char

nth = flip (!!)



type Key = String
type Encrypted = String



encrypt :: Key -> String -> Encrypted
encrypt key string = zipWith encryptChar keyCycle string
  where
  encryptChar = calcChar (+) pool
  keyCycle = cycle key

decrypt :: Key -> Encrypted -> String
decrypt key encrypted = zipWith decryptChar keyCycle encrypted
  where
  decryptChar = calcChar subtract pool
  keyCycle = cycle key

calcChar :: (Int -> Int -> Int) -> String -> Char -> Char -> Char
calcChar _ _ _ ' ' = ' '
calcChar shifter pool keyChar char = nth indexEnd pool
  where
  indexEnd = mod (shifter shiftAmount indexStart) poolSize
  shiftAmount = calcIndexWith pool keyChar
  indexStart = calcIndexWith pool char
  poolSize = length pool

calcIndexWith :: String -> Char -> Int
calcIndexWith string = fromJust . (flip elemIndex) string

pool :: String
pool = ['A'..'Z']

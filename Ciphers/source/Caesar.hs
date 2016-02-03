module Caesar where

import Data.Char
import Data.List
import Data.Maybe



type Shift = Int
type StringEncrypted = String



encrypt :: Shift -> String -> StringEncrypted
encrypt shift = fmap (encryptChar shift)

encryptChar :: Shift -> Char -> Char
encryptChar _ ' ' = ' '
encryptChar shift char = chars !! index
  where
  index = mod (fromJust (elemIndex (toUpper char) chars) + shift) poolSize



decrypt :: Shift -> StringEncrypted -> String
decrypt shift = fmap (decryptChar shift)

decryptChar :: Shift -> Char -> Char
decryptChar _ ' ' = ' '
decryptChar shift char = chars !! index
  where
  index = mod (fromJust (elemIndex (toUpper char) chars) - shift) poolSize



chars :: String
chars = ['A'..'Z']

poolSize :: Int
poolSize = length chars

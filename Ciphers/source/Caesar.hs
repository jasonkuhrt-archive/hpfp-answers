module Caesar where

-- The Caesar Cipher will encrypt text. The process is based upon
-- a Key which itself is also just text. The security model is
-- that as long as the Key is only known to trusted parties strangers
-- will not be able to dicpher the encrypted text.

-- First worked on in Chapter 11.
-- Revised in Chapter 13.

-- Encrypting Rules
-- ================

-- * Iteratively pair a character from the key with the text. If the key
--   becomes exhausted then restart from its beginning. Shift each text
--   character righward by the unicode order number of the key character.

-- * Space characters should not be encrypted.

import Data.Char



type Key = String
type StringEncrypted = String



caesarCipher :: Key -> String -> StringEncrypted
caesarCipher key = zipWith go (cycle key)
  where
  go :: Char -> Char -> Char
  go k ' '  = ' '
  go k s    = encryptChar k s

encryptChar :: Char -> Char -> Char
encryptChar keyChar char = chars !! index
  where
  chars = cycle ['A'..'Z']
  index = ord keyChar + ord char

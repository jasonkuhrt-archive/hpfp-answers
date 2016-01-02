module Caesar where

-- First worked on in Chapter 11.
-- Revised in Chapter 13.

-- The Caesar Cipher will encrypt text. The process is based upon
-- a Key which itself is also just text. The security model is
-- that as long as the Key is only known to trusted parties strangers
-- will not be able to read the StringEncrypted text.

import Data.Char



type Key = String
type StringEncrypted = String



-- If the key is exhausted then restart from its beginning.

caesarCipher :: Key -> String -> StringEncrypted
caesarCipher key = zipWith go (cycle key)
  where
  go k ' '  = ' '
  go k s    = encryptChar k s

encryptChar :: Char -> Char -> Char
encryptChar keyChar char = chars !! index
  where
  chars = cycle ['A'..'Z']
  index = ord keyChar + ord char

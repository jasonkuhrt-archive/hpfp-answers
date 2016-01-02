module Caesar where

-- First worked on in Chapter 11.
-- Revised in Chapter 13.

-- The Caesar Cipher will encrypt text. The process is based upon
-- a Key which itself is also just text. The security model is
-- that as long as the Key is only known to trusted parties strangers
-- will not be able to read the StringEncrypted text.



type Key = String
type StringEncrypted = String

caesarCipher :: Key -> String -> StringEncrypted
caesarCipher _ "" = ""
caesarCipher key string = go (cycle key) string
  where
  go ks (' ':xs)   = ' ' : go ks xs
  go (k:ks) (x:xs) = encryptChar k x : go ks xs

encryptChar :: Char -> Char -> Char
encryptChar keyChar char = chars !! index
  where
  chars = cycle ['A'..'Z']
  index = ord keyChar + ord char

module Main where

-- Command Line Interface for interacting with the Caesar Cipher.

import System.Exit (exitSuccess)
import Caesar



main :: IO ()
main = do
  putStrLn "==> Please enter a secret to use for encrypting."
  secret <- getLine
  putStrLn "==> Please enter text to encrypt."
  string <- getLine
  putStrLn "==> Your encrypted text is:"
  putStrLn $ caesarCipher secret string
  putStrLn "==> Would you like to encrypt again? (y/n)"
  string <- getLine
  case string of
    "y" -> main
    _   -> exitSuccess

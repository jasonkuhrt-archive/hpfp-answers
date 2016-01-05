module Main where

-- Command Line Interface for using the Caesar Cipher function interactively.

import qualified Caesar
import System.Exit (exitSuccess)



main :: IO ()
main = do
  putStrLn "==> Please enter a secret to use for encrypting."
  secret <- getLine
  putStrLn "==> Please enter text to encrypt."
  string <- getLine
  putStrLn "==> Your encrypted text is:"
  putStrLn $ Caesar.encrypt secret string
  putStrLn "==> Would you like to encrypt again? (y/n)"
  string <- getLine
  case string of
    "y" -> main
    _   -> exitSuccess

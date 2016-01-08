module Palindrome where

import Data.Char (toLower)
import System.Exit (exitSuccess)
import Control.Monad (forever)



main :: IO ()
main = forever $ do
  putStrLn "Please enter a Palindrome:"
  word <- getLine
  if isPalindrome word
    then do
      putStrLn "Yup, that's a Palindrome! Bye."
      exitSuccess
    else do
      putStrLn $ "Whoops, \"" ++ word ++ "\" is invalid. Try again. A Palindrome must have the same order of letters in either direction (e.g. mom)."
      main


meaingfulChars :: String
meaingfulChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

isPalindrome :: String -> Bool
isPalindrome string = stringMeaningful == reverse stringMeaningful
  where
  stringMeaningful =
    map toLower .
    filter (`elem` meaingfulChars) $
    string

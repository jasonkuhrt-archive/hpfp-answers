module Palindrome where

import System.Exit (exitSuccess)
import Control.Monad (forever)



main :: IO ()
main = forever $ do
  putStrLn "Please enter a Palindrome:"
  word <- getLine
  if word == reverse word
    then do
      putStrLn "Yup, that's a Palindrome! Bye."
      exitSuccess
    else do
      putStrLn $ "Whoops, \"" ++ word ++ "\" is invalid. Try again. A Palindrome must have the same order of letters in either direction (e.g. mom)."
      main

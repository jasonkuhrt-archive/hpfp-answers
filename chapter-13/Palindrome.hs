module Palindrom where

import System.Exit (exitSuccess)
import Control.Monad (forever)



main :: IO ()
main = forever $ do
  putStrLn "Please enter a palindrom:"
  word <- getLine
  if word == reverse word
    then do
      putStrLn "Yup, that's a palindrom! Bye."
      exitSuccess
    else do
      putStrLn $ "Whoops, \"" ++ word ++ "\" is invalid. Try again. A palindrom must have the same order of letters in either direction (e.g. mom)."
      main

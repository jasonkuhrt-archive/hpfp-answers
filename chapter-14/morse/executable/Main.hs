module Main where

import Control.Monad (forever, when)
-- import Data.List (intercalate)
-- import Data.Traversable (traverse)
import Morse (stringToMorse, morseToChar)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, stdin, hIsEOF)



main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] ->
      case arg of
        "from" -> convertFromMorse
        "to" -> convertToMorse
        _    -> argError
    _ -> argError
    where
    argError = do
      putStrLn "Please specify the first argument as being 'from' or 'to' morse, such as `> morse to`."
      exitFailure



convertFromMorse :: IO ()
convertFromMorse = forever $ do
  tryEnd
  -- Otherwise, proceed.
  line <- hGetLine stdin
  convertLine line

  where
  convertLine line = do
    let decoded :: Maybe String
        decoded = traverse morseToChar (words line)
    case decoded of
      (Just s) -> putStrLn s
      Nothing  -> do
        putStrLn $ "Error: Invalid Morse Code:" ++ line
        exitFailure



convertToMorse :: IO ()
convertToMorse = forever $ do
  tryEnd
  -- Otherwise, proceed.
  line <- hGetLine stdin
  convertLine line
  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        (Just string) ->
          putStrLn $ unwords string
        Nothing -> do
          putStrLn $ "Error: Morse code cannot represent this:" ++ line
          exitFailure



tryEnd :: IO ()
tryEnd = do
  isDone <- hIsEOF stdin
  when isDone exitSuccess

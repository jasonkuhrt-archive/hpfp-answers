module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Data.List
import Hangman




main :: IO ()
main = do
  werd <- randomWerd
  let game = createGame (fmap toLower werd)
  runGame game



-- Game IO

runGame :: Game -> IO ()
runGame game = forever $ do
  tryGameLose game
  tryGameWin game
  putStrLn (" " ++ show game)
  putStr "Guess a letter: "
  guessed <- getLine
  case guessed of
    [x] -> handleGuessChar game x >>= runGame
    _   -> putStrLn "Invalid guess. Your guess must be a single character. Try again."



tryGameLose :: Game -> IO ()
tryGameLose game@(Game werd mask guesses) =
  when (isGameLose game) $ do
    putStrLn "You lose!"
    putStrLn ("The word was \"" ++ werd ++ "\".")
    exitSuccess

tryGameWin :: Game -> IO ()
tryGameWin game@(Game werd mask guesses) =
  when (isGameWin game) $ do
    putStrLn "You win!"
    exitSuccess



handleGuessChar :: Game -> Char -> IO Game
handleGuessChar game c = do
  putStrLn $ "You have guessed \"" ++ [c] ++ "\"."
  case (isHit game c, isAlreadyGussed game c) of
    (_, True) -> do
      putStrLn "That character has already been guessed."
      return game
    (True, _) -> do
      putStrLn "Hit!"
      return (guessChar game c)
    (False, _) -> do
      putStrLn "Miss!"
      return (guessChar game c)



randomWerd :: IO Werd
randomWerd = gameWerdList >>= randomItem



-- Word List Utilities

-- randomWerdFromList :: WerdList -> IO Werd
-- randomWerdFromList werdList = do
--   i <- randomItem werdList
--   return (werdList !! i)

readWerdList :: IO WerdList
readWerdList = do
  dict <- readFile "data/dict.txt"
  return (lines dict)






-- General Utilities

-- TODO: This will throw an exception on empty lists. Error occurs with `!!`.
--       `index` will equal -1 which is an invalid index.
randomItem :: [a] -> IO a
randomItem xs = do
  index <- randomIndex xs
  return (xs !! index)
  -- Also, here is an alternative solution/style. It is more
  -- concise but requires more understanding from tbe user.
  -- liftM (xs !!) (randomIndex xs)

randomIndex :: [a] -> IO Int
randomIndex xs = randomRIO (0, length xs - 1)



gameWerdList :: IO WerdList
gameWerdList = do
  werds <- readWerdList
  return (filter fitsGame werds)
  where
    fitsGame :: Werd -> Bool
    fitsGame werd = let l = length werd
                     in l >= gameMinWerdLength &&
                        l <= gameMaxWerdLength

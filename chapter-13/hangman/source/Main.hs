module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Data.List



-- Type `Word` is already taken so we use the terminology "werd" in place
-- of "word" throughout this program.

type Werd = String
type WerdList = [Werd]

data Game = Game Werd [Maybe Char] String

instance Show Game where
  show (Game werd mask guesses) =
    intersperse ' ' (stringifyMask mask) ++ " | Letters guessed so far: " ++ stringifyGuesses guesses






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



gameWerdList :: IO WerdList
gameWerdList = do
  werds <- readWerdList
  return (filter fitsGame werds)
  where
    fitsGame :: Werd -> Bool
    fitsGame werd = let l = length werd
                     in l >= gameMinWerdLength &&
                        l <= gameMaxWerdLength






-- Game Graphics

stringifyMask :: [Maybe Char] -> String
stringifyMask = fmap stringifyMaskItem

stringifyMaskItem :: Maybe Char -> Char
stringifyMaskItem Nothing     = '_'
stringifyMaskItem (Just char) = char

stringifyGuesses :: String -> String
stringifyGuesses ""     = "(None)"
stringifyGuesses string = string






-- Game Logic

createGame :: Werd -> Game
createGame werd = Game werd (fmap (const Nothing) werd) []



guessChar :: Game -> Char -> Game
guessChar (Game werd mask guesses) c =
  Game werd maskUpdated guessesUpdated

  where

  guessesUpdated =
    c : guesses

  maskUpdated =
    zipWith (zipper c) werd mask

  zipper guessedChar werdChar charMask =
    if guessedChar == werdChar
    then Just werdChar
    else charMask



isHit :: Game -> Char -> Bool
isHit (Game werd _ _) char = char `elem` werd

isAlreadyGussed :: Game -> Char -> Bool
isAlreadyGussed (Game _ _ guesses) char = char `elem` guesses

isGameLose :: Game -> Bool
isGameLose (Game _ _ guesses) = length guesses > 7

isGameWin :: Game -> Bool
isGameWin (Game _ mask _) = all isJust mask

gameMinWerdLength :: Int
gameMinWerdLength = 5

gameMaxWerdLength :: Int
gameMaxWerdLength = 10






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

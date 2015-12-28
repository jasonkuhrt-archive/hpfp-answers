module Main where

import Control.Monad (forever)
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
main = undefined



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

freshGame :: Werd -> Game
freshGame werd = Game werd (fmap (const Nothing) werd) []



isHit :: Game -> Char -> Bool
isHit (Game werd _ _) char = char `elem` werd



isAlreadyGussed :: Game -> Char -> Bool
isAlreadyGussed (Game _ _ guesses) char = char `elem` guesses



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

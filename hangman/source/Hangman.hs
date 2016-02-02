module Hangman where

import Data.Maybe (isJust)
import Data.List



-- Types

-- Type `Word` is already taken so we use the terminology "werd" in place
-- of "word" throughout this program.

type Werd = String
type WerdList = [Werd]

data Game =
  Game Werd [Maybe Char] String
  deriving (Eq)


instance Show Game where
  show (Game _ mask guesses) =
    intersperse ' ' (stringifyMask mask) ++ " | Letters guessed so far: " ++ stringifyGuesses guesses



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
guessChar game _
  | isGameOver game = game
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

isGameOver :: Game -> Bool
isGameOver game = isGameLose game || isGameWin game

isGameLose :: Game -> Bool
isGameLose (Game werd _ guesses) = length incorrectGuesses > 7
  where
    uniqueGuesses = nub guesses
    incorrectGuesses = uniqueGuesses \\ werd

isGameWin :: Game -> Bool
isGameWin (Game _ mask _) = all isJust mask

gameMinWerdLength :: Int
gameMinWerdLength = 5

gameMaxWerdLength :: Int
gameMaxWerdLength = 10

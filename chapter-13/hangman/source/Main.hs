module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)



-- Type `Word` is already taken so we use the terminology "werd" in place
-- of "word" throughout this program.

type Werd = String
type WerdList = [Werd]



main :: IO ()
main = undefined



-- Game Logic

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

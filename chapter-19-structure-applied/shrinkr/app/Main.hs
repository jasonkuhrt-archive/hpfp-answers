{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty
import Data.Monoid ((<>))



main :: IO ()
main = genShortLink >>= print

-- We need to generate our shortened URIs that refer to the links people post to this service. In order to rename, we need a character pool to select from:

charPool :: String
charPool = ['A'..'Z'] <> ['0'..'9']

-- Now we need to pick random elements from the pool. We will need to be impure to achieve randomness:

randomElement :: String -> IO Char
randomElement xs = do
  let indexRange :: (Int, Int)
      indexRange = (0, length xs - 1)
  randomDigit <- SR.randomRIO indexRange
  return (xs !! randomDigit)

-- Next we apply randomElement to our char pool. By repeating this seven times we can make strings of seven random characters.

genShortLink :: IO String
genShortLink = replicateM 7 (randomElement charPool)

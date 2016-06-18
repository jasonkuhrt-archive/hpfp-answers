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

-- Next we need to persistently store shortLinks and the URIs they point to. We can model this by treating shortLinks as keys and the URI they point to as the key's value. Other data at play will be a connection to Redis, and a result of storing a shortLink (which lets us know if there was an error, otherwise the status after finishing the command).

saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortLink uri = R.runRedis conn (R.set shortLink uri)

-- Next we need a way to get at a URI via its shortLink.

getURI :: R.Connection
       -> BC.ByteString
       -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortLink = R.runRedis conn (R.get shortLink)

-- Next, we want any human to be easily able use this service. We'll need a graphical interface. We can create some simple view functions that return HTML templates which a browser can redner.

-- TODO

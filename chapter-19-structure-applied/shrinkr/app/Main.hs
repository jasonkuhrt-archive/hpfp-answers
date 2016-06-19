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
        -> IO (Either R.Reply Bool)
saveURI conn shortLink uri = do
  result <- R.runRedis conn (R.setnx shortLink uri)
  case result of
    -- Redis setnx semantics are that if the key already exists its value is not overwritten. We want these semantics because users should not be overwritting their own or others' shortLinks. A conflict is modelled here via False which states that while there was no unexpected error in trying to execute the command the key did exist and so no value was written. When this happens we should just try again. One conflict is already astronomically unlikely to happen, an loop/infinite loop of conflicts is infinitely more so, so this is a safe strategy I think! And we avoid doubling the cost of every request with a Redis `exists` check.
    Right False -> saveURI conn shortLink uri
    value -> return value

-- Next we need a way to get at a URI via its shortLink.

getURI :: R.Connection
       -> BC.ByteString
       -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortLink = R.runRedis conn (R.get shortLink)

-- Next are the web-service parts.

app :: R.Connection -> ScottyM ()
app redisConn = do
  get "/" $ do
    uri <- param "uri"
    let parsedURI :: Maybe URI
        parsedURI = parseURI (TL.unpack uri)
    case parsedURI of
      Nothing ->
        text (uri <> " is an invalid URI.")
      Just _  -> do
        shortLink <- liftIO genShortLink
        let shortLink' = BC.pack shortLink
            uri' = encodeUtf8 (TL.toStrict uri)
        response <- liftIO (saveURI redisConn shortLink' uri')
        text . TL.pack $ shortLink
  get "/:shortLink" $ do
    shortLink <- param "shortLink"
    uri <- liftIO (getURI redisConn shortLink)
    case uri of
      Left reply     ->
        text . TL.pack . show $ reply
      Right maybeURI ->
        case maybeURI of
          Nothing         ->
            text "URI not found"
          Just byteString ->
              text
            . TL.fromStrict
            . decodeUtf8
            $ byteString

-- Next we need an entry point into this application. This is what happens once upon boot.

main :: IO ()
main = do
  redisConnection <- R.connect R.defaultConnectInfo
  scotty 3000 (app redisConnection)

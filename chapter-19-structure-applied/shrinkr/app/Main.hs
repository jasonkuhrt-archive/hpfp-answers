{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM, (<=<))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as Redis
import Database.Redis (Redis)
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty
import Data.Monoid ((<>))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Reader (ReaderT, ask, runReaderT)



-- We need to generate our shortened URIs that refer to the links people post to this service. In order to rename, we need a character pool to select from:

charPool :: String
charPool = ['A'..'Z'] <> ['0'..'9']

-- Now we need to pick random elements from the pool. We will need to be impure to achieve randomness:

randomElement :: String -> IO Char
randomElement xs = fmap (xs !!) randomIndex
  where
  randomIndex = SR.randomRIO indexRange
  indexRange :: (Int, Int)
  indexRange = (0, length xs - 1)

-- Next we apply randomElement to our char pool. By repeating this seven times we can make strings of seven random characters.

genShortLink :: IO String
genShortLink = replicateM 7 (randomElement charPool)



-- Next we need to persistently store shortLinks and the URIs they point to. We can model this by treating shortLinks as keys and the URI they point to as the key's value. Other data at play will be a connection to Redis, and a result of storing a shortLink (which lets us know if there was an error, otherwise the status after finishing the command).

saveURI :: BC.ByteString
        -> BC.ByteString
        -> ReaderT Redis.Connection IO (Either Redis.Reply Bool)
saveURI shortLink uri = do
  conn <- ask
  let
    -- Redis setnx semantics are that if the key already exists its value is not overwritten. We want these semantics because users should not be overwritting their own or others' shortLinks. A conflict is modelled here via False which states that while there was no unexpected error in trying to execute the command the key did exist and so no value was written. When this happens we should just try again. One conflict is already astronomically unlikely to happen, an loop/infinite loop of conflicts is infinitely more so, so this is a safe strategy I think! And we avoid doubling the cost of every request with a Redis `exists` check.
    go = ensureSaved <=< redisRun (Redis.setnx shortLink uri)
    -- TODO ...we need to generate a new shortLink!
    ensureSaved (Right False) = go conn
    ensureSaved value = return value
  Reader.ReaderT go



-- Next we need a way to get at a URI via its shortLink.

getURI :: BC.ByteString -> ReaderT Redis.Connection IO (Either Redis.Reply (Maybe BC.ByteString))
getURI = Reader.ReaderT . redisRun . Redis.get



-- Next are the web-service parts.

app :: Redis.Connection -> ScottyM ()
app conn = do

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
        response <- liftIO (Reader.runReaderT (saveURI shortLink' uri') conn)
        text . TL.pack $ shortLink

  get "/:shortLink" $ do
    shortLink <- param "shortLink"
    uri <- liftIO (Reader.runReaderT (getURI shortLink) conn)
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



-- Next we need an entry point into this application. This is happens upon launch.

main :: IO ()
main = do
  conn <- Redis.connect Redis.defaultConnectInfo
  scotty 3000 (app conn)



-- Helpers

redisRun :: Redis a -> Redis.Connection -> IO a
redisRun = flip Redis.runRedis

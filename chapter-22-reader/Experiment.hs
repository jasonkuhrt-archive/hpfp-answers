module Experiment where

-- import Data.Monoid ((<>))
import Control.Monad.Trans.Reader


test1 :: Reader String String
test1 = fmap (++ "!") ask

test2 :: Monad m => String -> ReaderT String m String
test2 string = do
  prefix <- ask
  return (prefix ++ string)
  -- return ((++ string) =<< ask)

test3 :: Monad m => String -> ReaderT String m (IO ())
test3 string = fmap (print . (++ string)) ask

-- OR
-- test3 string = ask >>= return . print . (++ string)

-- OR
-- test3 string = do
--   prefix <- ask
--   return $ print (prefix ++ string)


test4 :: (Show r, Monad m) => String -> ReaderT r m (IO ())
test4 description = fmap (print . (++ description) . show) ask

test5 :: (Show r) => String -> ReaderT r IO ()
test5 description = ReaderT (print . (++ description) . show)

main :: IO ()
main = do
  print $ runReader test1 "hello"
  print $ runReader (test2 "good") "not-"
  -- TODO In the following two examples I do not fully undertand how `runReader` here is operating on a ReaderT type.
  runReader (test3 "good") "not-"
  runReader (test4 " is a number") (1 :: Int)
  runReaderT (test5 " is a number") (1 :: Int)

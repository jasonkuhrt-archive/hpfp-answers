module Ask where

newtype Reader a b = Reader { runReader :: a -> b }



ask :: Reader a a
ask = Reader id

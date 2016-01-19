module Properties where

import Test.QuickCheck



main :: IO ()
main = runQuickCheck

runQuickCheck :: IO ()
runQuickCheck = quickCheck additionIsGreater

additionIsGreater :: Integer -> Bool
additionIsGreater n = n + 1 < n

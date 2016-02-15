module HelloWorld where

import Control.Applicative
import Data.Monoid



db1 :: [(Integer, String)]
db1 = [(3, "hello"), (4, "julie"), (5, "kbai")]

db2 :: [(Integer, String)]
db2 = [(7, "sup?"), (8, "chris"), (9, "aloha")]

db3 :: [(Integer, Integer)]
db3 = [(2,3), (5,6), (7,8)]

db4 :: [(Integer, Integer)]
db4 = [(4,10), (8,13), (1,9001)]



store1 :: Integer -> Maybe String
store1 x = lookup x db1

store2 :: Integer -> Maybe String
store2 x = lookup x db2

store3 :: Integer -> Maybe Integer
store3 x = lookup x db3

store4 :: Integer -> Maybe Integer
store4 x = lookup x db4

module Combinations where

import Control.Applicative




type Stops = String
type Vowels = String
type Combinations = [(Char, Char, Char)]

stops :: String
stops = "pbtdkg"


vowels :: String
vowels = "aeiou"


makeCombos :: Stops -> Vowels -> Stops -> Combinations
makeCombos = liftA3 (,,)
-- Alternative:
-- makeCombos stops1 vowels stops2 = (,,) <$> stops1 <*> vowels <*> stops2

combos :: Combinations
combos = makeCombos stops vowels stops

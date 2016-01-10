module Main where



-- Types

type Name = String
type Age = Integer

data Person =
  Person Name Age
  deriving (Show)

data PersonInvalid =
    NameEmpty
  | AgeToLow
  | PersonInvalidUnknown String
  deriving (Show, Eq)



-- IO

main :: IO ()
main = undefined



-- Logic

makePerson :: Name -> Age -> Either PersonInvalid Person
makePerson name age
  | isValid       = Right $ Person name age
  | isNameEmpty   = Left NameEmpty
  | isAgeNegative = Left AgeToLow
  | otherwise     = Left $ PersonInvalidUnknown $
                        "Name was: " ++ show name ++ "\n" ++
                        "Age was: " ++ show age
  where
    isValid       = not isNameEmpty && not isAgeNegative
    isNameEmpty   = name == ""
    isAgeNegative = age < 0

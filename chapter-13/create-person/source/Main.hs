module Main where



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



main :: IO ()
main = promptForPerson



-- IO

promptForPerson :: IO ()
promptForPerson = do
  putStrLn "Simple Person Record Creator\n"
  putStrLn "==> Please enter the perons's name:"
  name <- getLine
  putStrLn "==> Please enter their age:"
  ageString <- getLine
  case makePerson name (read ageString :: Integer)  of
    (Right person)       -> putStrLn $
      "==> SUCCESS: Record created: " ++ show person
    (Left personInvalid) -> putStrLn $
      "==> ERROR: Failed to create record. The error type was: " ++ show personInvalid



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

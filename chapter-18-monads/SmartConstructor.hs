module SmartConstructor where



type Direction = String
type Speed = Float
type Age = Float

data Storm =
  Storm {
    direction :: Direction,
    speed :: Speed,
    age :: Age
  }
  deriving (Eq, Show)


makeStorm :: Direction -> Speed -> Age -> Maybe Storm
makeStorm d s a = do
  d' <- validateDirection d
  s' <- validateSpeed s
  a' <- validateAge a
  pure $ Storm d' s' a'

-- Alternative:
  -- validateDirection d >>= \d' ->
  --   validateSpeed s >>= \s' ->
  --     validateAge a >>= \a' ->
  --       return $ Storm d' s' a'

validateDirection :: Direction -> Maybe Direction
validateDirection = validate (/= "")

validateSpeed :: Speed -> Maybe Speed
validateSpeed = validate (>= 0)

validateAge :: Age -> Maybe Age
validateAge = validate (>= 0)



-- lib

validate :: (a -> Bool) -> a -> Maybe a
validate f x = if f x then Just x else Nothing

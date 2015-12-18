module OnlyNatural where



data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)



natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

test1 = [
  natToInteger Zero,
  natToInteger (Succ Zero),
  natToInteger (Succ (Succ Zero))
  ]



integerToNat :: Integer -> Maybe Nat
integerToNat integer
  | integer < 0 = Nothing
  | otherwise   = Just (go integer)
  where
  go integer
    | integer == 0 = Zero
    | otherwise    = Succ (go (integer - 1))

test2 = [
  integerToNat 0,
  integerToNat 1,
  integerToNat 2,
  integerToNat (-1)
  ]

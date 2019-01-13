data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)


natToInteger :: Nat -> Integer
natToInteger = go 0
  where
    go n (Succ z) = go (n+1) z
    go n Zero = n
  
integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just (go i)
    where go 0 = Zero
          go n = Succ (go $ n - 1)

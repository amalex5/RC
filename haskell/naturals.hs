
data Nat = Zero | Succ Nat
           deriving Show

natToInteger :: Nat -> Integer
natToInteger = head . m
	where m Zero = [0]
	      m (Succ n) = [sum [x | x <- (1: m n)]]

integerToNat :: Integer -> Nat
integerToNat (n+1) = Succ (integerToNat n)
integerToNat 0 = Zero

-- scratch2
import Data.List
import Data.Char


data Nat = Zero | Succ Nat
          deriving Show

-- test: map natToInteger [Zero,Succ (Zero),Succ (Succ (Zero))]

-- test: map integerToNat [0,1,2,3]

--data Tree = Leaf Integer | Node Tree Integer Tree

--occurs :: Integer -> Tree -> Bool
--occurs m (Leaf n) = m == n
--occurs m (Node l n r)
--	= case compare m n of
--		LT -> occurs m l
--		EQ -> True
--		GT -> occurs m r

---- occurs 5 (Node (Node (Leaf 4) 5 (Leaf 6)) 7 (Leaf 10))

--balanced :: Tree -> Bool
--leaves (Leaf _) = 1
--leaves (Node l r) = leaves l + leaves r
--balanced (Leaf _) = True
--balanced (Node l r) = abs (leaves l - leaves r) && balanced l && balanced r

data Tree2 = Leaf Integer | Node Tree2 Tree2 deriving Show
balance :: [Integer] -> Tree2
halve xs = splitAt (length xs `div` 2) xs
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
			where (ys, zs) = halve xs
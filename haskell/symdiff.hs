-- symdiff



data Expr = Val Int
		   | Const [Char] -- :: [Char] -> Expr
		   | Symbol
		   | Neg Expr
           | Add Expr Expr
           | Mul Expr Expr
           | Sub Expr Expr
           | Pow Expr Expr
           | Div Expr Expr
           | Exp Expr
           | Log Expr
           | Sin Expr 
           | Cos Expr
           deriving (Show,Eq)

diff :: Expr -> Expr
diff (Val _ ) = Val 0
diff (Const _ ) = Val 0
diff (Symbol) = Val 1
diff (Neg x) = Neg (diff x)
diff (Add x y) = Add (diff x) (diff y)
diff (Mul x y) = Add (Mul (diff x) y ) (Mul x (diff y))
diff (Sub x y) = diff (Add x (Neg y))
diff (Pow x (Val n)) = Mul (Val n) (Pow x (Val (n - 1) ) )
diff (Div x y) = diff (Mul x (Pow y (Val (-1)) ) )
diff (Exp x) = Mul (Exp x) (diff x)
diff (Sin x) = Mul (Cos x) (diff x)
diff (Cos x) = Neg (Mul (Sin x) (diff x) )

simplify :: Expr -> Expr
simplify (Neg (Val 0)) = Val 0
simplify (Neg (Val x)) = Val (-x)
simplify (Neg (Neg x)) =  simplify x
simplify (Neg x) = Neg (simplify x)
simplify (Add x y)
  | simplify x == (Val 0) = simplify y
  | simplify y == (Val 0) = simplify x
  | simplify x == simplify y = Mul (Val 2) (simplify x)
  | otherwise = (Add (simplify x) (simplify y) ) 
simplify (Mul x y)
  | simplify x == (Val 1) = simplify y
  | simplify y == (Val 1) = simplify x
  | simplify x == (Val 0) = Val 0
  | simplify y == (Val 0) = Val 0
  | simplify x == simplify y = (Pow x (Val 2))
  | otherwise = (Mul (simplify x) (simplify y) )
simplify Symbol = Symbol
simplify (Const x) = Const x
simplify (Val x) = Val x
simplify (Pow x y)
  | y == (Val 1) = x
  | y == (Val 0) = Val 1
  | otherwise = Pow x y
simplify (Cos x) = Cos (simplify x)
simplify (Sin x) = Sin (simplify x)


-- redo this so it's some pretty tree un-parsing with operational priority!
pprint :: Expr -> [Char]
pprint (Neg x) = "-" ++ pprint x
pprint (Add x y) = pprint x ++ "+" ++ pprint y
pprint (Mul Symbol (Val x)) = show x ++ "x"
pprint (Mul (Val x) Symbol) = show x ++ "x"
pprint (Mul x y) = pprint x ++ "*" ++ pprint y
pprint (Pow x y) = pprint x ++ "^" ++ pprint y
pprint Symbol = "x"
pprint (Const x) = x
pprint (Val x) = show x
pprint (Cos x) = "cos(" ++ pprint x ++ ")"
pprint (Sin x) =  "sin(" ++ pprint x ++ ")"


--(Neg (Cos (Mul Symbol (Val 5))))
--instance (Show) => Show (Expr) where
--   show (Leaf x) = show x
--   show (Branch val l r) = " " ++ show val ++ "\n" ++ show l ++ "  " ++ show r


--(Add (Sin (Mul (Val 5) (Pow Symbol (Val 7)))) (Mul (Val 4) (Neg Symbol)) )

-- GADTS
--test :: Expr -> Bool
-- diff x = undefined
--diff (* x y) = (+ (* x (diff y)) (* (diff x) y))
--diff (+ x y) = (+ (diff x) (diff y))
--diff (- x y) = diff (+ x (-y))
--diff (exp x) = (exp (diff x))

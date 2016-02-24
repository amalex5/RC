-- monad sketchpad
-- from reading along: http://www.cs.nott.ac.uk/~pszgmh/monads
import Prelude hiding (Maybe,Just,Nothing,(>>=))

inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n^2 : sqr ns

inc2 = map (+1)

sqr2 = map (^2)

data Expr  = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

-- test it out: 
-- eval (Val 6)
-- >> 6
-- eval (Div (Val 6) (Val 2))
-- >> 3

data Maybe a = Nothing | Just a deriving (Show)

safediv :: Int -> Int -> Maybe Int
safediv n m = if m == 0 then Nothing else Just (n `div` m)

eval2 :: Expr -> Maybe Int
eval2 (Val n) = Just n
eval2 (Div x y) = case eval2 x of
					Nothing -> Nothing
					Just n -> case eval2 y of
								Nothing -> Nothing
								Just m -> safediv n m

-- try it out: eval2 (Div (Val 6) (Val 2))
-- >> Just 3

seqn :: Maybe a -> Maybe b -> Maybe (a,b)
seqn Nothing _ = Nothing
seqn _ Nothing = Nothing
seqn (Just x) (Just y) = Just (x,y)

eval3 :: Expr -> Maybe Int
eval3 (Val n) = Just n
eval3 (Div x y) = apply f (eval3 x `seqn` eval3 y)
                    where f (n,m) = safediv n m

apply :: (a -> Maybe b) -> Maybe a -> Maybe b
apply f Nothing = Nothing
apply f (Just x) = f x

(>>=~) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>=~ _ = Nothing
(Just x) >>=~ f = f x

eval4 :: Expr -> Maybe Int
eval4 (Val n) = Just n
eval4 (Div x y) = eval4 x >>=~ (\ n ->
	              eval4 y >>=~ (\ m -> 
	              	safediv n m))

eval5 :: Expr -> Maybe Int
eval5 (Val n) = Just n
eval (Div x y) = do n <- x
					m <- y
					safediv n m





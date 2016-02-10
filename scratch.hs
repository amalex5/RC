-- scratch pad for writing haskell stuff

double x = x + x
dub x = x + x
quadruple x = double $ double x

factorial n = product [1..n]
average ns = sum ns `div` length ns

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * (product xs)

add' :: Int -> (Int -> Int)
add' x y =  x + y

twice f x = f $ f x
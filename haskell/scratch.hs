-- scratch pad for writing haskell stuff
import Prelude hiding ((^),and,concat,replicate,(!!),all,any,map,filter,curry)

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

--let bob = [1..10]

safetail :: [a] -> [a]
safetail = \ xs -> 
    case xs of
    	[] -> []
    	(_ : xs) -> xs

remove n xs = take n xs ++ drop (n+1) xs

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

twinPrime :: Int -> [Int]
twinPrime n = if (prime (n+2) || prime (n-2)) then [n] else []

twinPrimes :: Int -> [Int] -- make a list of all the twin primes up to n
twinPrimes n = [x | xs <- (primes n), x <- (twinPrime xs)]


find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
  where n = length xs - 1

addFive x n
  | x > 10 =  x + adder
  | x < 10 = x + 5
  where
  	adder = 6 + n

divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

divisors :: Int -> [Int]
divisors x = [ d | d <- [1..x], x `divides` d]

(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * m ^ (n-1) 

and :: [Bool] -> Bool
and [] = True
and (b:bs) = b && and bs

concat :: [[a]] -> [a]
concat [] = []
concat (xs : xss) = xs ++ concat xss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

(!!) :: [a] -> Int -> a
(x: _) !! 0 = x
(_ : xs) !! n = (xs) !! (n - 1)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = 
	if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort zs) (msort ys)
  where (zs, ys) = halve xs

all :: (a -> Bool) -> [a] -> Bool
all p = foldr (&&) True . map p

any :: (a -> Bool) -> [a] -> Bool
any p xs = foldr (\ x acc -> (p x) || acc) False xs

map :: (a -> b) -> [a] -> [b]
map f = foldl (\ xs x -> xs ++ [f x]) []
 
filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\ x xs -> if p x then x:xs else xs) []

dec2int :: [Integer] -> Integer
dec2int = foldl (\ x y -> 10*x + y) 0

curry :: ((a,b) -> c) -> a -> b -> c
curry f = \ x y -> f (x,y)

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

type Bit = Int
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

int2bin2 :: Int -> [Bit]
int2bin2 = unfold (==0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop8alt :: [Bit] -> [[Bit]]
chop8alt = unfold (\x -> length x == 0) (take 8) (drop 8) 







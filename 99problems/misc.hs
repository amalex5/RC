-- miscellaneous haskell stuff as a work along in learn you a haskell!

doubleMe x = x + x  
doubleUs x y = x*2 + y*2
doubleSmallNumber x = if x < 100
  then x*2
  else x
doubleSmallNumber' x = (if x>100 then x else x*2) + 1
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

lucky:: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!!!!"
lucky x = "No luck :(:("

factorial:: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

third:: (a,b,c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "empty lists have no head!"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num b) => [a] -> b
sum' [] = 0
sum' (x:xs) = 1 + sum' xs

capital :: String -> String
capital "" = error "no text in an empty string!"
capital everything@(x:xs) = "The init of " ++ everything ++ " is " ++ ((xs))

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | a < b = LT

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi < 18.5 = "underweight!"
  | bmi < 25 = "normal!"
  | bmi < 30 = "overweight!"
  | otherwise = "obese!"
  where bmi = weight / height^2 

-- calcBmis :: (RealFloat a) => [(a,a)] -> [(a,a,a)]
-- calcBmis xs = [(w, h, bmi w h) | (w,h) <- xs]
--  where bmi weight height = weight / height ^2

-- calcBmis' :: (RealFloat a) => [(a,a)] -> String
-- calcBmis' (w,h) = "A weight of " ++ (show w) ++ "and a height of " ++ (show h) ++ "begets a BMI of "
--  where bmi weight height = weight / height ^2

myDouble :: [a] -> [a]
myDouble [x] = [x]

calcBmis' :: (RealFloat a) => [(a,a)] -> [(a,a,a)]
calcBmis' x = [(w,h,bmi) | (w,h) <- x, let bmi = w/h^2]

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty lists have no max!"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

replicator :: (Num i, Ord i) => i -> a -> [a]
replicator n x
  | n <= 0 = []
  | otherwise = x:replicator (n-1) x

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys

-- gauss's childhood algorithm:
-- length (myZip [1..50] (reverse [50..100])) * 101

myElem :: (Eq a) => a -> [a] -> Bool
myElem k [] = False
myElem k (x:xs)
    | k == x = True
    | otherwise = myElem k xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ []  = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

largestDivisible :: (Integral a) => [a]
largestDivisible = take 20 (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

-- one way to find the sum of all the odd squares less than 10,000:
-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- another way:
-- sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])  

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n = n:chain (n*3 + 1)

-- counts the number of collatz chains of length greater than y starting at x or smaller
numLongChains ::  Int -> Int -> Int -- why did I have to use Int? it wouldn't let me set Integral as typeclass without error...
numLongChains x y = length (filter (isLong y) (map chain [1..x]))
  where
  	isLong :: Int -> [a] -> Bool
  	isLong y xs =  y < length xs

mySum :: (Num a) => [a] -> [a]
mySum xs = foldl (\acc x -> acc ++ [5]) [4] xs


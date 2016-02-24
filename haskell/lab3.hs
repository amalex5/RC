module Lab3 where

-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

evens :: [Integer] -> [Integer]
evens  = filter (\x -> x `mod` 2 == 0)


-- complete the following line with the correct type signature for this function
squares :: Integer -> [Integer]
squares n = map (\x -> x*x) [1..n]


sumSquares :: Integer -> Integer
sumSquares n =  sum . squares $ n

-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
squares' :: (Enum a, Num a) => Int -> a -> [a]
squares' m n = take m $ map (\x -> x*x) [n+1..]

sumSquares' :: Int -> Int
sumSquares' x = sum . uncurry squares' $ (x, x)


-- ===================================
-- Ex. 8
-- ===================================

coords :: Integer -> Integer -> [(Integer,Integer)]
coords m n= [(x,y) | x <- [0..m], y <- [0..n]]

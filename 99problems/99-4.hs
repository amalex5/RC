-- 99 problems, number 4
-- find the number of elements in a list!

myNum :: [a] -> Int
myNum [] = 0
myNum (x:xs) = 1 + myNum xs
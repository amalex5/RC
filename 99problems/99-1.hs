-- 99 problems, #1
-- find the last element of a list

myLast :: [a] -> a
myLast [] = error "empty list!"
myLast (x:[]) = x
myLast (x:xs) = myLast xs
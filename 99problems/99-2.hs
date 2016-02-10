-- 99 problems, #2
-- find the penultimate element of a list

myPenultimate :: [a] -> a
myPenultimate [] = error "list is empty!"
myPenultimate [x]  = error "list must have more than one thing!"
myPenultimate (x:y:[]) = x
myPenultimate (x:xs) = myPenultimate xs
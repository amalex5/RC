-- 99 problems, number 5
-- reverse a list!

myReverse :: [a] -> [a]
myReverse x = helper x []
  where
  	helper :: [a] -> [a] -> [a]
  	helper [] acc = acc
  	helper (x:xs) acc = helper xs (x:acc)
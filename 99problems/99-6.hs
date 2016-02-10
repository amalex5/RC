-- 99 haskell problems, #6
-- is a list a palindrome?!?

-- let's use the reverse method we made in #5
-- (unless there's a better algorithm?)
myReverse :: [a] -> [a]
myReverse x = helper x []
  where
  	helper :: [a] -> [a] -> [a]
  	helper [] acc = acc
  	helper (x:xs) acc = helper xs (x:acc)

-- now to the content:
isPalindrome :: (Eq a) => [a] -> Bool -- this is interesting: it wouldn't compile unless i swore that [a] was a member of the Eq typeclass!
isPalindrome x
  | x == myReverse x = True
  | otherwise = False

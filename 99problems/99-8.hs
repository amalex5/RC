-- 99 problems, #8!
-- delete consecutive identical elements in a list
-- move things from one list to the other, ignoring if they're repeats

eliminator :: (Eq a) => [a] -> [a]
eliminator x = helper x []
  where 
    helper :: (Eq a) => [a] -> [a] -> [a]
    helper [] acc = acc
    helper (x:y:xs) acc
      | x == y = helper (x:xs) acc
      | otherwise = helper (y:xs) (acc ++ [x])
    helper (x:[]) acc = acc ++ [x]

-- working but... it reverses the list?!? wtf? switching the ++ order doesn't fix
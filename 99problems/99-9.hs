-- 99 haskell problems, #9
-- pack consecutive duplicated elements of a list into a sublist

data NestedList a = Elem a | List [NestedList a]

packer :: (Eq a) => [a] -> NestedList a
packer x = helper x []
  where
  	helper [] acc = acc
  	helper (x:y:xs) acc
      | x \= y = packer (y:xs) (acc ++ [x])
      | x == y =  newList ([x]++[y]) xs
        where
        	newList :: (Eq a) => a -> a -> [a] -> [a]
        	newlist x y [] = x ++ y
        	newlist x y z:zs
        	  | y == z = newList -

--packer x = helper x []
--  where 
--    helper :: (Eq a) => [a] -> [a] -> NestedList a
--    helper [] acc = acc
--    helper (x:y:xs) acc
--      | x == y = helper (y:xs) acc:x
--      | otherwise = helper [] acc
--    helper (x:[]) acc = acc:x
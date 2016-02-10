-- 99 haskell problems, #7
-- flatten a potentially-nested list!
-- note that because lists in haskell are homogenous, we need a new data type to handle a nested list
-- example input: flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a ->  [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List xs) = foldr (\x acc -> (flatten x) ++ acc) [] xs


-- flatten (List []) = []
-- flatten (Elem x) = Elem x
-- flatten (List xs) = []
-- flatten (List xs) = foldr (\x acc -> (flatten x) ++ acc) [] xs


-- foldr ((++) x) []
-- flatten :: NestedList a -> [a]
-- flatten (Elem a   )   = [a]
-- flatten (List (x:xs)) = f 
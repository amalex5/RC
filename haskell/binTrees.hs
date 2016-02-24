-- binary trees!

data Tree = Leaf Int | Node Tree Int Tree 
-- node is a left subtree, a value at that node, and a right subtree

--Node 
--  (Node (Leaf 1) 3 (Leaf 4))
--  5
--  (Node (Leaf 6) 7 (Leaf 49)

--(Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 49)) )

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = n == m
occurs m (Node l n r) = m == n
                        || occurs m l
                        || occurs m r
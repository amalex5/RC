-- writing some algorithms with trees!

data Tree a = Leaf a | Node a [Tree a]
  deriving (Show)

testTree = Node 5 [Node 3 [Leaf 1, Leaf 2], Node 4 [Leaf 5, Leaf 6] ] :: Tree Int
-- this doesn't work if i don't explicitly declare types??

--breadthFirst :: () -> tree a

-- returns a list of all elements satisfying pred
depthFirstSearch :: (a -> Bool) -> Tree a -> [a]
depthFirstSearch pred (Leaf x) = case pred x of
	True -> [x]
	False -> []
depthFirstSearch pred (Node x xs) = (depthFirstSearch pred (Leaf x)) ++ (concatMap (depthFirstSearch pred) xs)

-- try it out:
test1 = depthFirstSearch (>0) testTree

-- let's make a stack!

type Stack a = [a]

push :: a -> Stack a -> ((),Stack a)
push x xs = ((),x:xs)

pop :: Stack a -> (a,Stack a)
pop (x:xs) = (x,xs)

peek :: Stack a -> (a,Stack a)
peek (x:xs) = (x,x:xs)

instance Functor Tree where
	fmap f (Node z xs) = Node (f z) (map (fmap f) xs)
	fmap f (Leaf x) = Leaf (f x)

-- now let's implement breadth-first search!
breadthFirstSearch :: (a -> Bool) -> Tree a -> [a]
breadthFirstSearch pred (Leaf x) = case pred x of
	True -> [x]
	False -> []
breadthFirstSearch pred (Node x xs) = (breadthFirstSearch pred (Leaf x))

searchNodes :: (a -> Bool) -> [Tree a] -> [a]
searchNodes = undefined

searchKids :: (a -> Bool) -> [Tree a] -> [a]
searchKids pred xs = filter (pred . returner) xs

mpred :: a -> Bool
mpred x = undefined

returner :: Tree a -> a
returner (Node z _) = z
returner (Leaf z  ) = z





-- balance a binary tree!
-- determine if a bin tree is balanced!







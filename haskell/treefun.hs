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
searchKids pred xs = undefined -- filter (pred . returner) xs

mpred :: a -> Bool
mpred x = undefined

returner :: Tree a -> a
returner (Node z _) = z
returner (Leaf z  ) = z


-- fun with binary trees!
-- from 99 problems in haskell
-- problems 54A-60

data BinaryTree a = Empty | Branch a (BinaryTree a) (BinaryTree a)
              deriving (Show, Eq)

-- shorthand constructor
leaf x = Branch x Empty Empty

-- problem 55: write a function "to construct completely balanced binary trees for a given number of nodes"
cbalTree :: Int -> BinaryTree Char
cbalTree 0 = Empty
cbalTree 1 = Branch 'x' (cbalTree 0) (cbalTree 0)
cbalTree n = case even n of
	True  ->   Branch 'x' (cbalTree (halfN + 1)) (cbalTree halfN)
	False ->   Branch 'x' (cbalTree  halfN     ) (cbalTree halfN)
	where halfN = (n-1) `div` 2

-- problem 56: write a predicate to check whether a given binary tree is symmetric
-- like above, "we are only interested in the structure, not in the contents of the nodes."
mirror :: BinaryTree a -> BinaryTree a -> Bool
mirror Empty Empty = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror _ _ = False

symmetric t = mirror t t

-- problem 57: Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers.
-- not sure what that all means, but we'll just try to build a binary search tree
-- which we'll do by making a fxn that inserts something into a bst
-- and then another function that folds that over a list
insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert x Empty = Branch x Empty Empty
insert x (Branch c l r) = case compare x c of
	EQ -> Branch c       l            r
	GT -> Branch c       l      (insert x r)
	LT -> Branch c (insert x l)       r

testBST = Branch 10 (Branch 8 (Branch 2 Empty Empty) (Branch 9 Empty Empty)) (Branch 15 (Branch 11 Empty Empty) (Branch 300 (Branch 299 Empty Empty) Empty))
testBST2 = Branch 8 (Branch 7 (Branch 6 Empty Empty) Empty) (Branch 9 Empty (Branch 10 Empty Empty))

buildBST :: (Ord a) => [a] -> BinaryTree a
buildBST xs = foldr insert Empty xs
-- whoa so easy! <3 folding

testNums = [10,9,2,8,15,300,299,11]

compareBST :: (Ord a) => BinaryTree a -> BinaryTree a -> Ordering
compareBST Empty Empty = EQ
compareBST (Branch a _ _) (Branch b _ _) = compare a b

isBST :: (Ord a) => BinaryTree a -> Bool
isBST Empty = True
isBST (Branch c Empty Empty) = True
isBST (Branch c (Branch l _ _)        Empty   ) = c > l
isBST (Branch c       Empty     (Branch r _ _)) = c < r
isBST (Branch c l@(Branch x _ _) r@(Branch y _ _)) = c > x && c < y && isBST l && isBST r
-- this seems way too verbose. surely i can make it less ugly.

-- note that buildBST doesn't create balanced binary trees
-- well, because insert doesn't neccessarily balance them
-- um, let's write a function to see if a BST is balanced
-- we'll need a helper function first
depth :: BinaryTree a -> Int
depth Empty = 0
depth (Branch c Empty Empty) = 0
depth (Branch c l r) = 1 + ( max (depth l) (depth r) )

isBalanced :: (Ord a) => BinaryTree a -> Bool
isBalanced     Empty      = True
isBalanced (Branch c l r) = l `similarDepth` r && 
                            isBalanced l       && 
                            isBalanced r
   where
   	similarDepth x y = (depth x)     == depth y ||
                       (depth x) + 1 == depth y ||
                       (depth x) - 1 == depth y

-- let's write a function to delete a node from a binary tree!
delete :: (Ord a) => a -> BinaryTree a -> BinaryTree a
delete x Empty = Empty
delete x (Branch c l r) = case c of 
	x -> undefined -- merge them
	otherwise -> Branch c (delete x l) (delete x r)



--- in which andrew plays in the forest!

import Data.Monoid
import Data.Foldable
import Control.Monad

-- let's define our tree!
data BinaryTree a = Leaf a | Node (BinaryTree a) a (BinaryTree a)

-- let's make it foldable! 
instance Foldable BinaryTree where
  foldMap f (Leaf x) = f x
  foldMap f (Node l n r) = (foldMap f l) `mappend` f n `mappend` (foldMap f r)

-- now that we've made it foldable, we can use some built-in foldable functions!
-- for example, here's a test tree:

testTree1 :: BinaryTree Int
testTree1 = (Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 49)) )

-- if we add up all the numbers in this tree, we should get 75!
-- we can use the built in "sum" function! works on any foldable!

foldabilityTest :: Bool
foldabilityTest = sum testTree1 == 75

-- but that's already built-in and boring. what if we want to fold more awesomely?
-- suppose we do this. we take a tree of strings:

testTree2 :: BinaryTree [Char]
testTree2 = (Node (Node (Leaf "Paul") "Jim" (Leaf "Jean")) "Andrew" (Node (Leaf "Brian") "Sarah" (Leaf "Janet")))

-- er, wait, [Char]s are set up as monoids by default in haskell. hm.
-- well, anyway, we can like concatenate them or something!
-- i guess we need to use foldr!

foldrabilityTest = (foldr mappend ""  testTree2) == "PaulJimJeanAndrewBrianSarahJanet"

-- but let's include spaces:
foldrabilityTest2 = foldr (\x -> (\y ->  x ++ " " ++ y)) ""  testTree2
-- doesn't return a boolean, but you get the idea
-- the nested lambdas are kind of gross, though
-- and we could use mappend instead of ++
-- (if we wanted to reinforce the idea that strings are monads under concatenation
foldrabilityTest3 = foldr (\x -> (\y ->  x `mappend` " " `mappend` y)) ""  testTree2
-- and mempty instead of ""
foldrabilityTest4 = foldr (\x -> (\y ->  x `mappend` " " `mappend` y)) mempty  testTree2
-- i guess if we really wanted to do this cleanly, we could re-define mappend to include a space between things
-- or just define a seperate function and call it there:
spaceyConcat :: [Char] -> [Char] -> [Char]
x `spaceyConcat` "" = x
"" `spaceyConcat` y = y
x `spaceyConcat` y = x ++ " " ++ y
-- yeah, and note how due to pattern-matching, we can easily take care of that edge case!
-- so now we can redo this:
foldrabilityTest5 = foldr spaceyConcat mempty testTree2
-- whee!

--- blah. now let's just write some random tests for binary trees.

depth :: BinaryTree a -> Int
depth t = 

depth1 :: BinaryTree a -> Int -> Int
depth (Leaf _) 0 = 1
depth (Node l n r) = if depth l > depth r then depth l else depth r



-- but, screw the concatenation
-- what if we want to visualize this tree AS a tree?
-- and not as a structureless list, nor an unreadable nest of parentheses
-- oooh, and this will be a good oppurtuninty to implement a breadth-first something
--showTree :: BinaryTree a -> [Char]
--showTree t =  showTree1 t []

--showTree (Leaf)
--showTree1 (Node l n r) l = 

--   4   
--  / \
-- 2   3



-- well, we have to have something to fold it over.
-- we need a MONOID! but what shall we make into a monoid?
-- integers under addition!

--instance Int a => Monoid (Sum a) where
--  mempty = Sum 0
--  x `mappend` y = Sum (unSum x + unSum y)






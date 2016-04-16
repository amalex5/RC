-- the game of life in haskell!!!

import Data.Monoid

data Cell = Live | Dead
  deriving (Eq)

instance Show Cell where
	show Live = "X"
	show Dead = "."

data List2D a = List2D [ [a] ]
  deriving (Eq,Ord)

height :: List2D a -> Int
height (List2D x) = length x

width :: List2D a -> Int
width (List2D (x:xs)) = length x

data Coords a = Coords (a,a)
  deriving (Show,Eq,Ord)

--instance Functor Coords where
--	fmap f@(g,h) (Coords (x,y)) = Coords (g x, h y)

instance Monoid (List2D a) where
    mempty = List2D [[]]
    mappend (List2D x) (List2D y) = List2D (x++y)

instance Functor List2D where
	fmap = list2Dmap

list2Dmap :: (a->b) -> List2D a -> List2D b
list2Dmap f (List2D []) = List2D []
list2Dmap f (List2D (x:xs)) = (List2D [list2Dmap' f x]) <> (list2Dmap f (List2D xs))

list2Dmap' :: (a->b) -> [a] -> [b]
list2Dmap' f [] = []
list2Dmap' f (z:zs) = (f z) : (list2Dmap' f zs)



instance (Show a) => Show (List2D a) where
	show = showList2D

showList2D :: (Show a) => List2D a -> [Char]
showList2D (List2D []) = ""
showList2D (List2D (x:xs)) = (showList2D' x) ++ "\n" ++ (showList2D (List2D xs))

showList2D' [] = ""
showList2D' (z:zs) = (show z) ++ (showList2D' zs)
-- ANDREW YOU'RE WRITING THE SAME GODDAMN RECURSIVE FUNCTIONS OVER AND OVER
-- MAKE IT SIMPLER




-- THERE'S GOT TO BE A BETTER WAY THAN WRITING ALL THIS STUFF BY HAND
-- IS IT A FUNCTOR OR SOMETHING?!?!?
--composeTuple :: (a -> a, a -> a) -> (a,a) -> (a, a)
--composeTuple (f,g) (x,y) = (f x, g y)

--mapTuple :: [(a->a,a->a)] -> (a,a) -> [(a,a)]
--mapTuple (x:xs) (y,z) = mapTuple' (x:xs) (y,z) []

--mapTuple' :: [(a->a,a->a)] -> (a,a) -> [(a,a)] -> [(a,a)]
--mapTuple' [] (y,z) acc = acc
--mapTuple' (x:xs) y acc = (composeTuple x y):(mapTuple' xs y acc)


getIJth :: List2D a ->  Coords Int -> a
(List2D x@(z:zs)) `getIJth` (Coords (i,j)) = (x !! (j `mod` (length x))) !! (i `mod` (length z))

neighborCoords :: List2D a -> Coords Int -> [Coords Int]
neighborCoords   b (Coords (x,y)) = [(Coords (h,k)) | h <- perms (width b) x, 
                                                      k <- perms (height b) y, 
                                                      (h,k) /= (x,y) ]
                                      where perms l z = map (\x -> (x+z) `mod` l) [-1..1] 

neighborContents :: List2D a -> Coords Int -> [a]
neighborContents b (Coords (x,y)) = map (getIJth b) (neighborCoords b (Coords (x,y)))

numLiveNeighbors :: List2D Cell -> Coords Int ->  Int
numLiveNeighbors b c = length (filter (\x -> x == Live) (neighborContents b c))

judge :: List2D Cell -> Coords Int -> Cell
judge b c = case b `getIJth` c of
	Live -> case numLiveNeighbors b c of
		2 -> Live
		3 -> Live
		otherwise -> Dead
	Dead -> case numLiveNeighbors b c of
		3 -> Live
		otherwise -> Dead

--instance Functor Coords where
--	--fmap :: (a -> b) -> f a -> f b
--	fmap f (Coords (x,y)) = Coords ( (composeTuple f) (x,y) )

--neighborCoords :: Coords Int -> List2D a -> [Coords Int]
--neighborCoords t@(Coords (i,j)) (List2D x) = filter (\x -> x /= Coords (i,j)) (map (\x -> Coords x) (mapTuple tuplesOfPermutations (i,j)))
-- ALL OF THIS IS STUPID
-- I SHOULD BE ABLE TO DO THIS WITH ALL THE BUILT IN HASKELL STUFF
-- WHY AM I WRITING MY OWN FUNCTOR-NESS
-- put mod length here
-- Vec???

--neighbors :: Coords Int -> List2D a -> [a]
--neighbors coords arr = map (\x -> arr `getIJth` x) (neighborCoords coords arr)
--- OH ANDREW FOR THE LOVE OF GOD YOU CAN MAKE THIS NICER

--tuplesOfPermutations :: [(Int -> Int, Int -> Int)]
--tuplesOfPermutations = [(x,y) | x <- listOfPermutations, y <- listOfPermutations ]

----listOfPermutations = [(1+), (0+), ((-1)+)] 
----[(\x -> x+1), (\x -> x+0), (\x -> x-1)]
--listOfPermutations :: [Int -> Int]
--listOfPermutations = [ (\x -> (x + 1) `mod` boardSize),
--                       (\x -> (x + 0) `mod` boardSize),
--                       (\x -> (x - 1) `mod` boardSize)
--                      ]
--boardSize = 7
-- (\x -> (x + 1) `mod` length)
--- recursively generate a new List2D by folding over the old
-- judge c b 



-- judge by making up all the pairs of cells
-- and map or whatever over THAT
--allCoords = [ Coords (x,y) | x<- [0..(boardSize-1)], y<- [0..(boardSize-1)] ]


--numDeadNeighbors :: Coords Int -> List2D Cell -> Int
--numDeadNeighbors c b = length (filter (\x -> x == Dead) (neighbors c b))

testList2 = List2D [
                    [Dead,Dead,Dead,Dead,Dead,Dead,Dead],
                    [Dead,Dead,Dead,Dead,Dead,Dead,Dead],
                    [Dead,Dead,Dead,Live,Dead,Dead,Dead],
                    [Dead,Dead,Dead,Live,Dead,Dead,Dead],
                    [Dead,Dead,Dead,Live,Dead,Dead,Dead],
                    [Dead,Dead,Dead,Dead,Dead,Dead,Dead],
                    [Dead,Dead,Dead,Dead,Dead,Dead,Dead],
                    [Dead,Dead,Dead,Dead,Dead,Dead,Dead]
                    ]
--plop :: Coords Int
plop = Coords (3,2) :: Coords Int
-- so this will give us a list of all the neighboring coordinates:
test1 = neighborCoords  testList plop
-- and this will give us their contents:
testGetContents = map (\x -> testList `getIJth` x) test1
-- CHRIST ANDREW THIS IS SO AWFUL



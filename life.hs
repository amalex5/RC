-- the game of life in haskell!!!


data Cell = Live | Dead
  deriving (Eq)

instance Show Cell where
	show Live = "X"
	show Dead = "."

data List2D a = List2D [ [a] ]
  deriving (Eq,Ord)

data Coords a = Coords (a,a)
  deriving (Show,Eq,Ord)

instance (Show a) => Show (List2D a) where
	show = showList2D

showList2D :: (Show a) => List2D a -> [Char]
showList2D (List2D []) = ""
showList2D (List2D (x:xs)) = (showList2D' x) ++ "\n" ++ (showList2D (List2D xs))

showList2D' [] = ""
showList2D' (z:zs) = (show z) ++ (showList2D' zs)
-- ANDREW YOU'RE WRITING THE SAME GODDAMN RECURSIVE FUNCTIONS OVER AND OVER
-- MAKE IT SIMPLER

getIJth :: List2D a ->  Coords Int -> a
(List2D x) `getIJth` (Coords (i,j)) = (x !! j) !! i

-- THERE'S GOT TO BE A BETTER WAY THAN WRITING ALL THIS STUFF BY HAND
-- IS IT A FUNCTOR OR SOMETHING?!?!?
composeTuple :: (a -> a, a -> a) -> (a,a) -> (a, a)
composeTuple (f,g) (x,y) = (f x, g y)

mapTuple :: [(a->a,a->a)] -> (a,a) -> [(a,a)]
mapTuple (x:xs) (y,z) = mapTuple' (x:xs) (y,z) []

mapTuple' :: [(a->a,a->a)] -> (a,a) -> [(a,a)] -> [(a,a)]
mapTuple' [] (y,z) acc = acc
mapTuple' (x:xs) y acc = (composeTuple x y):(mapTuple' xs y acc)

--instance Functor Coords where
--	--fmap :: (a -> b) -> f a -> f b
--	fmap f (Coords (x,y)) = Coords ( (composeTuple f) (x,y) )

neighborCoords :: Coords Int -> List2D a -> [Coords Int]
neighborCoords t@(Coords (i,j)) (List2D x) = filter (\x -> x /= Coords (i,j)) (map (\x -> Coords x) (mapTuple tuplesOfPermutations (i,j)))
-- ALL OF THIS IS STUPID
-- I SHOULD BE ABLE TO DO THIS WITH ALL THE BUILT IN HASKELL STUFF
-- WHY AM I WRITING MY OWN FUNCTOR-NESS

-- Vec???

neighbors :: Coords Int -> List2D a -> [a]
neighbors coords arr = map (\x -> arr `getIJth` x) (neighborCoords coords arr)
--- OH ANDREW FOR THE LOVE OF GOD YOU CAN MAKE THIS NICER

tuplesOfPermutations :: [(Int -> Int, Int -> Int)]
tuplesOfPermutations = [(x,y) | x <- listOfPermutations, y <- listOfPermutations ]

listOfPermutations :: [Int -> Int]
listOfPermutations = [(\x -> x+1), (\x -> x+0), (\x -> x-1)]

--- recursively generate a new List2D by folding over the old

judge :: Coords Int -> List2D Cell -> Cell
judge c b = case b `getIJth` c of
	Live -> case numLiveNeighbors c b of
		2 -> Live
		3 -> Live
		otherwise -> Dead
	Dead -> case numDeadNeighbors c b of
		3 -> Live
		otherwise -> Dead





numLiveNeighbors :: Coords Int -> List2D Cell -> Int
numLiveNeighbors c b = length (filter (\x -> x == Live) (neighbors c b))

numDeadNeighbors :: Coords Int -> List2D Cell -> Int
numDeadNeighbors c b = length (filter (\x -> x == Dead) (neighbors c b))

testList = List2D [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
testList2 = List2D [[Dead,Live,Dead,Dead,Dead],[Dead,Live,Live,Dead,Dead],[Dead,Dead,Live,Dead,Dead]]
--plop :: Coords Int
plop = Coords (1,2) :: Coords Int
-- so this will give us a list of all the neighboring coordinates:
test1 = neighborCoords plop testList
-- and this will give us their contents:
testGetContents = map (\x -> testList `getIJth` x) test1
-- CHRIST ANDREW THIS IS SO AWFUL



-- the game of life in haskell!!!
-- with sweet type-aliasing
-- and no hand-coded neighbor enumerating!

import Data.Array

type LifeBoard = Array Coords Cell

type Coords = (Int,Int)

data Cell = Live | Dead
  deriving (Eq)

instance Show Cell where
  show Live = "#"
  show Dead = " "

-- | stupid helper functions that should be built-in
arrWidth a  = fst . snd $ bounds a
arrHeight a = snd . snd $ bounds a

-- | given a 'LifeBoard' and coordinates, return a list of the coordinates of all the neighbors
-- this function is my favorite part of this code :)
-- no hand-enumeration!
-- also, note that that ONE usage of 'mod' gives the board toroidal geometry
neighborCoords ::  LifeBoard -> Coords -> [Coords]
neighborCoords arr (x,y) = [ (i,j) | i <- perms (arrWidth  arr) x,
                                     j <- perms (arrHeight arr) y,
                                     (i,j) /= (x,y)
                                ]
                            where perms size var = map (\x -> (x+var) `mod` size) [-1..1]

-- | given a 'LifeBoard' and some coordinates, return a list of the live/deadness of the neighboring cells
neighborContents :: LifeBoard -> Coords -> [Cell]
neighborContents arr coords = map (arr!) (neighborCoords arr coords)

-- | given a 'LifeBoard' and the coordinates of one of its cells, return the number of live neighbors
numLiveNeighbors :: LifeBoard -> Coords -> Int
numLiveNeighbors arr coords = length (filter ((==) Live) (neighborContents arr coords))

-- | given a 'LifeBoard;, and the coordinates of one of its cells
-- determine whether that cell lives or dies in the next generation
-- i really like this function, too. 
judgeCell :: LifeBoard -> Coords -> Cell
judgeCell arr coords = case arr ! coords of
  Live -> case numLiveNeighbors arr coords of
    2 -> Live
    3 -> Live
    otherwise -> Dead
  Dead -> case numLiveNeighbors arr coords of
    3 -> Live
    otherwise -> Dead

-- | given a 'LifeBoard, compute its next generation
evolveBoard :: LifeBoard -> LifeBoard
evolveBoard arr =  array ((0,0),(width,height)) 
                     [ ((x,y),c) | x <- [0..width], 
                                   y <- [0..height],
                                   let c = judgeCell arr (x,y)
                      ]
                  where
                    width  = arrWidth arr
                    height = arrHeight arr

-- | returns a potentially-infinite list of arrays of 'LifeBoard's
-- each array being a subsequent generation evolved from 'seed'
allTheStates :: LifeBoard -> [LifeBoard]
allTheStates seed = iterate evolveBoard seed

-- | a little hacky. pretty-printer for 'LifeBoard'.
printArr  :: LifeBoard -> String 
printArr arr = unlines [unwords [show (arr ! (x, y)) | x <- [0..(arrWidth arr)]] | y <- [0..(arrHeight arr)]]

-- | print the first 'n' generations of the Game of Life, starting with 'seed'
printN :: Int -> LifeBoard -> IO ()
printN n seed = mapM_ putStrLn $ map (printArr) (take n $ allTheStates seed)



-- examples & tests & stuff!!!

-- | initializes a new board of size x by y full of dead cells
deadBoard :: Int -> Int -> LifeBoard
deadBoard x y = array ((0,0),(x,y)) [((i,j),Dead) | i<-[0..x],j<-[0..y]  ]

spinnerExample = (deadBoard 15 15) // [ ((2,3),Live),
                                        ((2,4),Live),
                                        ((2,5),Live) 
                                      ]

gliderExample :: LifeBoard
gliderExample = (deadBoard 15 15) // [ ((1,0), Live),
                                       ((2,1), Live),
                                       ((0,2), Live),
                                       ((1,2), Live),
                                       ((2,2), Live)
                                      ]


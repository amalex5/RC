-- the game of life in haskell!!!

import Data.Array

type LifeBoard = Array Coords Cell

type Coords = (Int,Int)

data Cell = Live | Dead
  deriving (Eq)

instance Show Cell where
	show Live = "#"
	show Dead = " "


arrWidth a  = fst . snd $ bounds a
arrHeight a = snd . snd $ bounds a

neighborCoords ::  LifeBoard -> Coords -> [Coords]
neighborCoords arr (x,y) = [ (i,j) | i <- perms (arrWidth  arr) x,
                                     j <- perms (arrHeight arr) y,
                                     (i,j) /= (x,y)
                                ]
                            where perms size var = map (\x -> (x+var) `mod` size) [-1..1]

neighborContents :: LifeBoard -> Coords -> [Cell]
neighborContents arr (x,y) = map (arr!) (neighborCoords arr (x,y))

numLiveNeighbors :: LifeBoard -> Coords -> Int
numLiveNeighbors arr (x,y) = length (filter (\x -> x == Live) (neighborContents arr (x,y)))

-- | Given an array representing a Game of Life board, and the coordinates of one of its cells
-- determine whether that cell lives or dies in the next generation
judgeCell :: LifeBoard -> Coords -> Cell
judgeCell arr c@(x,y) = case arr ! c of
  Live -> case numLiveNeighbors arr c of
    2 -> Live
    3 -> Live
    otherwise -> Dead
  Dead -> case numLiveNeighbors arr c of
    3 -> Live
    otherwise -> Dead

-- | Given an array representing a Game of Life board,
-- compute the next generation
evolveArr :: LifeBoard -> LifeBoard
evolveArr arr =  array ((0,0),(width,height)) 
                  [ ((x,y),c) | x <- [0..width], 
                                y <- [0..height],
                                let c = judgeCell arr (x,y)  ]
                  where
                    width  = arrWidth arr
                    height = arrHeight arr

-- | Returns a potentially-infinite list of arrays
-- each array being a subsequent generation of the Game of Life evolved from 'seed'
allTheStates :: LifeBoard -> [LifeBoard]
allTheStates seed = iterate evolveArr seed

-- | A little hacky. Pretty-printer for arrays
printArr  :: LifeBoard -> String 
printArr arr = unlines [unwords [show (arr ! (x, y)) | x <- [0..(arrWidth arr)]] | y <- [0..(arrHeight arr)]]

-- | Prints the first 'n' generations of the Game of Life, starting with 'seed'
printN :: Int -> LifeBoard -> IO ()
printN n seed = mapM_ putStrLn $ map (printArr) (take n $ allTheStates seed)



-- examples & tests & stuff!!!

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


-- the game of life in haskell!!!

import Data.Array

data Cell = Live | Dead
  deriving (Eq)

instance Show Cell where
	show Live = "#"
	show Dead = " "

arrWidth a  = fst . snd $ bounds a
arrHeight a = snd . snd $ bounds a

neighborCoords :: (Enum e, Num e,Integral e, Ix e) => Array (e, e) a -> (e,e) -> [(e,e)]
neighborCoords arr (x,y) = [ (i,j) | i <- perms (arrWidth  arr) x,
                                     j <- perms (arrHeight arr) y,
                                     (i,j) /= (x,y)
                                ]
                            where perms size var = map (\x -> (x+var) `mod` size) [-1..1]

neighborContents :: (Integral e, Ix e) => Array (e, e) a -> (e, e) -> [a]
neighborContents arr (x,y) = map (arr!) (neighborCoords arr (x,y))

numLiveNeighbors :: (Integral e, Ix e) => Array (e, e) Cell -> (e, e) -> Int
numLiveNeighbors arr (x,y) = length (filter (\x -> x == Live) (neighborContents arr (x,y)))

-- | Given an array representing a Game of Life board, and the coordinates of one of its cells
-- determine whether that cell lives or dies in the next generation
judgeCell :: (Integral e, Ix e) => Array (e, e) Cell -> (e, e) -> Cell
judgeCell arr c@(x,y) = case arr ! c of
  Live -> case numLiveNeighbors arr c of
    2 -> Live
    3 -> Live
    otherwise -> Dead
  Dead -> case numLiveNeighbors arr c of
    3 -> Live
    otherwise -> Dead

--mapJudge :: (Num t, Num a, Ix t, Ix a) => Array (a, t) e1 -> [((a, t), e)] -> Array (a, t) e
--mapJudge arr = array ((0,0),(arrWidth arr,arrHeight arr)) 

-- | Given an array representing a Game of Life board,
-- compute the next generation
evolveArr :: (Integral t, Ix t) => Array (t, t) Cell -> Array (t, t) Cell
evolveArr arr =  array ((0,0),(width,height)) 
                  [ ((x,y),c) | x <- [0..width], 
                                y <- [0..height],
                                let c = judgeCell arr (x,y)  ]
                  where
                    width  = arrWidth arr
                    height = arrHeight arr

-- | Returns a potentially-infinite list of arrays
-- each array being a subsequent generation of the Game of Life evolved from 'seed'
allTheStates :: (Integral t, Ix t) => Array (t, t) Cell -> [Array (t, t) Cell]
allTheStates seed = iterate evolveArr seed

-- | A little hacky. Pretty-printer for arrays
printArr  :: (Enum t, Enum b, Num t, Num b, Show a, Ix t, Ix b) => Array (t, b) a -> String 
printArr arr = unlines [unwords [show (arr ! (x, y)) | x <- [0..(arrWidth arr)]] | y <- [0..(arrHeight arr)]]

-- | Prints the first 'n' generations of the Game of Life, starting with 'seed'
printN :: (Integral b, Ix b) => Int -> Array (b, b) Cell -> IO ()
printN n seed = mapM_ putStrLn $ map (printArr) (take n $ allTheStates seed)


deadBoard x y = array ((0,0),(x,y)) [((i,j),Dead) | i<-[0..x],j<-[0..y]  ]

spinnerExample = (deadBoard 15 15) // [ ((2,3),Live),
                                        ((2,4),Live),
                                        ((2,5),Live) 
                                      ]

gliderExample = (deadBoard 15 15) // [ ((1,0), Live),
                                       ((2,1), Live),
                                       ((0,2), Live),
                                       ((1,2), Live),
                                       ((2,2), Live)
                                      ]


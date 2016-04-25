-- a more sophisticated implementation of the game of life
-- note that this needs the Data.QuadTree package
-- which i guess i installed "locally" with cabal? 

import Data.QuadTree


data Cell = Live | Dead
  deriving (Eq)

instance Show Cell where
  show Live = "#"
  show Dead = " "

data Quadcell = Quadcell {nw :: Int, ne :: Int, sw :: Int, se :: Int } deriving (Show,Eq)


data Macrocell = Quadcell | Macrocell {nw :: Macrocell,  ne :: Macrocell, sw :: Macrocell,  se :: Macrocell } deriving (Show,Eq)

result :: Macrocell -> Macrocell
result m = undefined


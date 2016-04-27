-- a more sophisticated implementation of the game of life
-- note that this needs the Data.QuadTree package
-- which i guess i installed "locally" with cabal? 

{-# LANGUAGE TemplateHaskell #-}

--import Data.QuadTree
import Control.Lens hiding (element)

data Cell = Live | Dead
  deriving (Eq)

instance Show Cell where
  show Live = "#"
  show Dead = " "

--data Quadcell = Quadcell {__nw :: Int, _ne :: Int, _sw :: Int, _se :: Int } deriving (Show,Eq)

data Macrocell = Leaf {_val ::Int} | Node {_nw :: Macrocell, 
                                  _ne :: Macrocell, 
                                  _sw :: Macrocell, 
                                  _se :: Macrocell }
                                  deriving (Show)


emptyQuad = Node { _nw = Leaf 0, _ne = Leaf 0, _sw = Leaf 0, _se = Leaf 0}
fullQuad  = Node { _nw = Leaf 1, _ne = Leaf 1, _sw = Leaf 1, _se = Leaf 1}
myM = Node { _nw = Leaf 1, _ne = Leaf 0, _sw = Leaf 1, _se = Leaf 0}
myN = Node {_nw = myM, _ne = myM, _sw = myM, _se = myM}
myR = Node {_nw = myN, _ne = myN, _sw = myM, _se = myM}

size :: Macrocell -> Int
size m = size' m 1

size' (Leaf _) acc = acc
size' (Node x _ _ _) acc = size' x (acc*2)

makeLenses ''Macrocell


result :: Macrocell -> Macrocell
result m = undefined


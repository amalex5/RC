{-# LANGUAGE TemplateHaskell #-}

-- playing with haskell lenses
--from https://hackage.haskell.org/package/lens-tutorial-1.0.1/docs/Control-Lens-Tutorial.html

import Control.Lens hiding (element)

data Atom = Atom {
	   _element :: String,
	   _point :: Point
} deriving (Show)

data Point = Point {
	_x :: Double,
	_y :: Double
} deriving (Show)

shiftAtomX :: Atom -> Atom
shiftAtomX (Atom e (Point x y)) = Atom e (Point (x+1) y)

-- OR BETTER

makeLenses ''Atom
makeLenses ''Point

shiftAtomX' :: Atom -> Atom
shiftAtomX' = over (point . x) (+ 1)

myAtom = Atom {_element = "C", _point = Point {_x=1.0, _y = 2.0}}

data Molecule = Molecule {_atoms :: [Atom] } deriving (Show)

makeLenses ''Molecule

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+1)

atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
atom2 = Atom { _element = "C", _point = Point { _x = 3.0, _y = 4.0 } }

myMolecule = Molecule {_atoms = [atom1,atom2]}








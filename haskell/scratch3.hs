-- scratch3

import Prelude hiding (Either,Left,Right)

--let's have some fun re-implementing type classes!

data Either a b = Left a | Right b
      deriving Show

instance Functor (Either e) where
	-- fmap :: (a -> b) -> f a -> f b
	fmap _ (Left x) = Left x
	fmap f (Right x) = Right (f x)
	-- we care about the right value and not the left

instance Applicative (Either e) where
    -- pure :: a -> f a --wrap it up!
    pure = Right
    --  (<*>) :: f (a -> b) -> f a -> f b
    Left f <*> _ = Left f
    Right f <*> y = fmap f y 
    -- why don't i have to send it y wrapped up?
    -- oh, because fmap takes care of that! so y is like "right x" or whatever
    -- but fmap can deal with just that!

-- exercise from this stackoverflow post:
-- http://stackoverflow.com/questions/10239630/where-to-find-programming-exercises-for-applicative-functors

data Triple a = Tr a a a 
--  given that, construct Applicative and Traversable instances!!!
-- which antecedently requires a Functor instance!

instance Functor Triple where 
	fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
	pure x = Tr x x x
	(Tr f g h) <*> (Tr x y z) = Tr (f x) (g y) (h z) 

-- still not really sure what traversable is, but here we go:
instance Traversable Triple where
	--traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f (Tr x y z) = 




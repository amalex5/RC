------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------
import Prelude hiding (Monoid,mappend,mempty,Foldable,foldMap)

data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================

root :: Rose a -> a 
root (x :> _) = x

children :: Rose a -> [Rose a]
children ( _ :> xs) = xs

testTree = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]
testTree2 = 'x' :> map (flip (:>) []) ['a'..'x']
testTree3 = 'x' :> map (\c -> c :> []) ['a'..'A']
testTree4 = 1 :> map (\c -> c :> []) [1..5]

ex2 = root . head . children . head . children . head . drop 2 $ children testTree

-- ===================================
-- Ex. 3-7
-- ===================================

size :: Rose a -> Int
size (x :> xs) = 1 + ( foldl (\acc z -> acc + size z) 0 xs )

leaves :: Rose a -> Int
leaves (x :> []) = 1
leaves (x :> xs) = sum (map leaves xs)

ex7 = (*) (leaves . head . children . head . children $ testTree) (product . map size . children . head . drop 2 . children $ testTree)


-- ===================================
-- Ex. 8-10
-- ===================================

instance Functor Rose where
  fmap f (x :> xs) = (f x) :> (map (fmap f) xs)

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) testTree

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a = Sum a deriving Show
newtype Product a = Product a deriving Show

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  x `mappend` y = Sum (unSum x + unSum y)
  
instance Num a => Monoid (Product a) where
  mempty = Product 1
  x `mappend` y = Product (unProduct x * unProduct y)

unSum :: Sum a -> a
unSum (Sum x) = x
unProduct :: Product a -> a
unProduct (Product x) = x

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))
  
num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))
  
ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

---- ===================================
---- Ex. 14-15
---- ===================================

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap :: Monoid m => (a -> m) -> (f a -> m)
  foldMap g a = fold $ fmap g a

instance Foldable [] where
  fold = foldr (mappend) mempty

instance Foldable Rose where
  --fold (x :> xs) = x `mappend` fmap fold xs
   --fold xs = fmap mappend xs
  fold (x :> xs) = x `mappend` (foldMap fold xs) 

tree = 1 :> [2 :> [], 3 :> [4 :> []]] 
tree' = fmap Product tree


sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))


---- ===================================
---- Ex. 16-18
---- ===================================

tree16 = 42 :> [3 :> [2:> [], 1 :> [0 :> []]]]

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) testTree) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ testTree) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ testTree))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) testTree) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ testTree) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ testTree))

---- ===================================
---- Ex. 19-21
---- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum x = unSum . fold $ fmap Sum x --unSum . fold 
fproduct x = unProduct . fold $ fmap Product x  --unProduct . fold

ex21 = ((fsum . head . drop 1 . children $ testTree) + (fproduct . head . children . head . children . head . drop 2 . children $ testTree)) - (fsum . head . children . head . children $ testTree)


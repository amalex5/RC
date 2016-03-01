-- bit diddler

import Prelude hiding (and)

data Bit = Int deriving (Eq,Show)

and :: Bit -> Bit -> Bit
1  `and` 1  = 1
_  `and` _  = 0

bitmapAnd :: [Bit] -> [Bit] -> [Bit]
bitmapAnd [] [] = []
bitmapAnd (x:xs) (y:ys) = x `and` y : bitmapAnd xs ys


xor :: Bit -> Bit -> Bit
1  `xor` 0 = 1
0 `xor` 1  = 1
_     `xor` _     = 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)
-- even cooler defn: int2bin2 = unfold (==0) (`mod` 2) (`div` 2)

bin2int :: [Bit] -> Int
bin2int x = bin2intHelper 0 x

bin2intHelper :: Int -> [Bit] -> Int
bin2intHelper b [] = b
bin2intHelper b (x:xs) = bin2intHelper ( b + x * 2 ^(length xs - 1) )  (tail xs)


numDiffBits :: Int -> Int -> Int
numDiffBits a b = killer 0 c where c = a `xor` b

killer :: Int -> Int -> Int
killer n 0 = n
killer n c = killer (n+1) (bin2int . killInsignificantestBit c)

killInsignificantestBit :: Int -> Int
illInsignificantestBit x = bin2int ( int2bin x `bitmapAnd` int2bin (x-1) )


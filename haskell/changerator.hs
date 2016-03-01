--changerator.hs

type Coin = Int
type Target = Int

--[1,5,10,25]

usCoins :: [Coin]
usCoins = [1,5,10,25]

value :: [Coin] -> Int
value = sum

numberOfWaysToMakeChangeWithoutOrdering :: Target -> Int
numberOfWaysToMakeChangeWithoutOrdering t = length . deduplicate [] . organize $ addCoins t [[]]

numberOfWaysToMakeChangeWithOrdering :: Target -> Int
numberOfWaysToMakeChangeWithOrdering t = length $ addCoins t [[]]

addCoins :: Target -> [[Coin]] -> [[Coin]]
addCoins t xs 
	| all (\x -> value x == t) xs = xs
	| otherwise = addCoins t . concat $ map (addCoin t) xs

addCoin :: Target -> [Coin] -> [[Coin]]
addCoin t xs
	| value xs == t = [xs]
	| otherwise = [ xs ++ [c] | c <- possibleCoins t usCoins xs  ]

possibleCoins :: Target -> [Coin] -> [Coin] -> [Coin]
possibleCoins t coins xs = filter (\x -> x <= (t - value xs))  coins

-- i don't like the rest of this code
-- the first part of the code generates a list of all the possible ways we can make change
-- and it does so using this pretty tree structure
-- BUT that means it returns tons of degenerate cases
-- i.e., it cares about the ORDER of the coins
-- but obviously a dime followed by penny is the same, for our purposes, as a penny followed by a dime
-- so this next chunk of code de-duplicates all the degenerate cases
-- but that's gross, because we should be able to do this in a way that doesn't create degenerate cases in the first place
-- and not have this two-stage process
-- (the internet says there's a dynamic programming solution that does that)

countCoins :: Coin -> [Coin] -> Int
countCoins c xs = length (filter (== c) xs)

organize :: [[Coin]] -> [[(Coin,Int)]]
organize [] = []
organize (x:xs) = [(c,n) | c <- usCoins, let n = countCoins c x ] : organize xs

deduplicate :: [[(Coin,Int)]] -> [[(Coin,Int)]] ->  [[(Coin,Int)]]
deduplicate xs [] = xs
deduplicate xs (y:ys) = deduplicate (y:xs) (filter (/= y) ys)




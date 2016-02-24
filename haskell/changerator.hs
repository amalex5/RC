--changerator.hs

type Coin = Int
type Target = Int

--[1,5,10,25]

value :: [Coin] -> Int
value = sum

numberOfWaysToMakeChangeWithOrdering :: Target -> Int
numberOfWaysToMakeChangeWithOrdering t = length $ addCoins t [[]]

addCoins :: Target -> [[Coin]] -> [[Coin]]
addCoins t xs 
	| all (\x -> value x == t) xs = xs
	| otherwise = addCoins t . concat $ map (addCoin t) xs

addCoin :: Target -> [Coin] -> [[Coin]]
addCoin t xs
	| value xs == t = [xs]
	| otherwise = [ xs ++ [c] | c <- possibleCoins t [1,5,10,25] xs  ]

possibleCoins :: Target -> [Coin] -> [Coin] -> [Coin]
possibleCoins t coins xs = filter (\x -> x <= (t - value xs))  coins

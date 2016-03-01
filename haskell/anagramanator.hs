-- take a list of strings
-- and turn them into a list of lists of strings, all of which are anagrams of each other
-- i.e., sort them into sublists by anagramability


test1 :: [[Char]]
test1 = [ "abc", "bac","cab","cat","hello world", "hollo werld"]

-- listicate test1
-- >> [ ["abc", "bac","cat"],["hello world"]]

listicate :: [[Char]] -> [[[Char]]]
listicate [] = []
listicate (x:xs) = [ g | g <- x:xs, isAnagram g x] : listicate (filter (not . isAnagram x) xs)

isAnagram :: [Char] -> [Char] -> Bool
isAnagram xs ys = sortChars ys == sortChars xs

-- quick quicksort implementation
sortChars :: [Char] -> [Char]
sortChars [] = []
sortChars (x:xs) = (sortChars lesser) ++ [x] ++ (sortChars greater)
	where
		lesser = filter (<x) xs
		greater = filter (>=x) xs 



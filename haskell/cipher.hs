-- modified caesar cipher from EdX's fxnl programming course
-- after some thought: easiest/cleanest way to do this is to represent capital letters as NEGATIVE integers!
-- all of the 1s are to avoid ambiguity at 0

import Data.Char

let2int :: Char -> Int
let2int c 
  | isUpper c = - (ord c - ord 'A' + 1)
  | otherwise = ord c - ord 'a' + 1


int2let :: Int -> Char
int2let n
	| n > 0 = chr (ord 'a' + n - 1)
	| otherwise = chr (ord 'A' - n - 1)

shift :: Int -> Char -> Char
shift n c 
	| isAlpha c = int2let ( shiftChar n c )
    | otherwise = c

shiftChar :: Int -> Char -> Int
shiftChar n c 
 | charInt > 0 = mod (charInt + n) 26
 | charInt < 0 = - (mod (- charInt + n) 26)
 where
 	charInt = let2int c

unShift :: Int -> Char -> Char
unShift n c
	| isAlpha c = int2let ( unShiftChar n c )
    | otherwise = c

unShiftChar :: Int -> Char -> Int
unShiftChar n c 
 | charInt > 0 = mod (charInt - n) 26
 | charInt < 0 = - (mod (- charInt - n) 26)
 where
 	charInt = let2int c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n xs = [ unShift n x | x <- xs]


-- test: encode 13 "Think like a Fundamentalist Code like a Hacker"
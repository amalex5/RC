-- write yourself a scheme in 48 hours!
--  from https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment



numberizeList :: [String] -> [Int]
numberizeList = map (\x -> read x :: Int)

main :: IO()
main = do 
	putStrLn "Gimme sum nummas to sum!"
	nums <- readLn -- getArgs is :: IO [String]
	putStrLn ("Hello, " ++ show (sum . numberizeList $ splitOn " " nums) )

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
	Left err -> "No match: " ++ show err
	Right val -> "found value."


-- $(x^2 + 5x).diff(x).add(x).eval(x=5)
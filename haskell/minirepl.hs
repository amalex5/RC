-- a tiny program to use the IO monad!

module Main where

import Data.Char(toUpper)
import Control.Monad

main :: IO ()
main = putStrLn "write your string: " >> fmap shout getLine >>= putStrLn

shout = map toUpper

test = "trapped!!!!!" >>= putStrLn
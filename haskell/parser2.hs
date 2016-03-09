-- parser2

module AndrewParser where

import Control.Monad

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero
--- stackoverflow's suggestion for fixing GHC's complaing about MonadPlus


newtype Parser a = P ( String -> [(a,String)] )

instance Monad Parser where
	return :: a -> Parser a
	return v   =  P (\inp -> [(v,inp)])
	(>>=) :: Parser a -> (a -> Parser b) -> Parser b
	p >>= f    =  P (\inp -> 
    	               case parse p inp of 
                       [(v,out)] -> parse (f v) out
                       [] -> []
    	                )

instance MonadPlus Parser where
    mzero                      =  P (\inp -> [])
    p `mplus` q                =  P (\inp -> case parse p inp of
                                                []        -> parse q inp
                                                [(v,out)] -> [(v,out)])


parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P ( \inp -> case inp of 
				  [] -> []
				  (x:xs) -> [(x,xs)]
				  )

failure :: Parser a
failure = mzero --using monoid-ness of parsers!

--return :: a -> Parser a
--return v = \inp -> [(v,inp)]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q --using the monoid-ness of parsers!


--p :: Parser (Char,Char)
--p = do x <- item
--       item
--       y <- item
--       return (x,y)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then
           	return x
           else
           	failure
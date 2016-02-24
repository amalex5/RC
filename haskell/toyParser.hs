-- toy parsers
import Prelude hiding (return)


type Parser a = String -> [(a, String)]

item :: Parser Char
item = \ inp -> case inp of
	    [] -> []
	    (x:xs) -> [(x,xs)]

failure :: Parser a
failure = \ inp -> []

return :: a -> Parser a
return v = \inp -> [(v,inp)]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \ inp -> case p inp of
	[] -> parse q inp
	[(v,out)] -> [(v,out)]

parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp

p :: Parser (Char, Char)
p = do x <- item
       item
       y <- item
       return (x,y)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then
              return x
           else
         	  failure

digit :: Parser Char
digit = undefined ---sat isDigit

char :: Char -> Parser Char
char x = sat (x ==)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)


-- tst parser!


import Text.ParserCombinators.Parsec
import Control.Monad

data Expr =    Var String
             | Val Int
             | Add Expr Expr
             | Mul Expr Expr
   deriving (Show)

parseAdd = do
	x <- parseExpr $ many1 alphaNum
	spaces >> char '+' >> spaces
	y <- parseExpr $ many1 alphaNum
	return $ Add ( x) ( y)

parseMul = do
    x <- many1 alphaNum
    spaces >> char '*' >> spaces
    y <- many1 alphaNum
    return $ Mul (Var x) (Var y)

parseVar :: Parser Expr
parseVar = do
	spaces
	x <- many1 alphaNum
	return $ Var x

parseVal :: Parser Expr
parseVal = liftM (Val . read) $ many1 digit

parseExpr :: Parser Expr
parseExpr = parseVal <|> 
            parseVar <|> 
            parseAdd <|>
            parseMul


readExpr :: String -> Expr
readExpr input = case parse parseExpr "lisp" input of
	Left err -> Var $ "No match: " ++ show err
	Right val -> val
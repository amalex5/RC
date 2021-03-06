
module Parser (parseExpr,parseWrappedExpressions,parseEquals)
where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Expr 

--data Expr = Val Integer 
--           | Var [Char]
--           | Exp Expr Expr
--           | Mul Expr Expr 
--           | Add Expr Expr
--           | Sub Expr Expr
--           | Div Expr Expr
--           | Fxn [Char] Expr
--          deriving (Show,Eq)

parseWrappedExpressions :: String -> [WrapperFxn]
parseWrappedExpressions inp = case parse manyWrappers "wrapper parser" inp of
  Left err -> [WrapperFxn ("Error","Error")] -- talk about shitty error handling!
  Right val -> val

parseEquals :: String -> (Expr,Expr)
parseEquals inp = case   parse equalityExpr "equality parser" inp of
  Left err -> (Error "the parser failed! :(", Error "yup")
  Right val -> val

parseExpr :: String -> Expr
parseExpr inp = case parse expr "expression parser" inp of
  Left err -> Error "the parser failed! :("
  Right val -> val

equalityExpr :: Parser (Expr,Expr)
equalityExpr = do 
                 a <- expr
                 char '='
                 b <- expr
                 return $ (a,b)

expr :: Parser Expr
expr = spaces >> buildExpressionParser table factor <?> "expression"

table :: [[ Operator Char st Expr ]]
table = [
    [ binary "^" Pow AssocRight], -- can't add "**" as an alternative because it conflicts with "*" as the multiplication operator. not sure how to easily fix without writing all my own parsing machinery...
    [ binary "*" Mul AssocLeft, binary "/" Div AssocLeft ],
    [ binary "+" Add AssocLeft, binary "-" Sub AssocLeft ]
    ]
  where
     binary  name fun assoc = Infix (do{ spaces; string name; spaces; return fun }) assoc
     prefix  name fun       = Prefix (do{ string name; return fun })
     postfix name fun       = Postfix (do{ string name; return fun })


factor = 
       try function -- "try" consumes no input, whereas <|> does. 
   <|> parens
   <|> numMulVar
   <|> number
   <|> variable
   <?> "simple expression"
   

   -- can we add something 


function :: Parser Expr
function = do 
           spaces
           name <- many1 letter
           char '('
           argument <- expr
           char ')'
           spaces
           return $ Fxn name argument

parens :: Parser Expr
parens = do
         char '('
         x <- expr
         char ')'
         return x

number :: Parser Expr
number = do { spaces; ds <- many1 digit; spaces; return (Val $ read ds) } <?> "number"

variable :: Parser Expr
variable = do
  spaces
  c <- many letter 
  spaces
  return $ Var c
--variable = many letter >>= return . Var 

numMulVar :: Parser Expr
numMulVar = do
  spaces
  d <- many1 digit
  c <- many1 letter
  spaces
  return $ Mul (Val $ read d) (Var c)


wrapper :: Parser WrapperFxn
wrapper = do 
           name <-  (many1 letter) <|> (string "$")
           char '('
           contents <- anyChar `manyTill` try endWrapper -- (char ').' <|> (char ')' >>= spaces eof )--probably Expr, but don't parse it yet
           --char ']'
           return $ WrapperFxn (name,contents)

endWrapper :: Parser ()
endWrapper = do
            char ')'
            lookAhead ( skipMany1 (char '.') <|> skipMany1 space <|> eof )

manyWrappers :: Parser [WrapperFxn]
manyWrappers = wrapper `sepBy` (char '.')





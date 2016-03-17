import Text.ParserCombinators.Parsec 
-- .String (Parser)
--import Text.Parsec.String.Char (anyChar)
--import Text.Parsec.String.Char
--import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Data.Char
--import Text.Parsec.String.Combinator (many1)

--data Parens = Val Integer |
--                Str String |
--                Parens 
--    deriving (Eq,Show)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

-- = case parse p "" of
--	Left err -> Str err
--	Right val -> val


numberExamples :: [(String,Integer)]
numberExamples = [("1", 1)
                 ,("23", 23)]

num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)

--parens :: Parser Parens
--parens = do
--    char '('
--    e <- many1 digit
--    char ')'
--    return (Val (read e))

data SingleAdd = SingleAdd Integer Integer
                 deriving (Eq,Show)


data Parentheses = Parentheses Integer
                   deriving (Eq,Show)


add :: Parser SingleAdd
add = do
    spaces
    e0 <- many1 digit
    spaces
    char '+'
    spaces
    e1 <- many1 digit
    spaces
    return (SingleAdd (read e0) (read e1))

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           spaces
           return x

parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof wrapper
  where
    wrapper = spaces >> p

parensL :: Parser Parentheses
parensL = do
    lexeme $ char '('
    e <- lexeme $ many1 digit
    lexeme $ char ')'
    return (Parentheses (read e))

data SimpleExpr = Num Integer
                | Var String
                | Add SimpleExpr SimpleExpr
                | Parens SimpleExpr
                  deriving (Eq,Show)

numE :: Parser SimpleExpr
numE = do
    n <- lexeme $ many1 digit
    return $ Num $ read n

varE' :: Parser SimpleExpr
varE' = do
    fc <- firstChar
    rest <- lexeme $ many nonFirstChar
    return $ Var (fc:rest)
  where
    firstChar = satisfy (\a -> isLetter a || a == '_')
    nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')

parensE :: Parser SimpleExpr
parensE = do
    lexeme $ char '('
    e <- lexeme $ many1 digit
    lexeme $ char ')'
    return $ Parens $ Num $ read e

parensE3 :: Parser SimpleExpr
parensE3 = do
    lexeme $ char '('
    e <- simpleExpr
    lexeme $ char ')'
    return $ Parens e

addE :: Parser SimpleExpr
addE = do
    e0 <- numE
    lexeme $ char '+'
    e1 <- numE
    return $ Add e0 e1


numOrVar :: Parser SimpleExpr
numOrVar = numE <|> varE'

simpleExpr :: Parser SimpleExpr
simpleExpr = numE <|> varE' <|> parensE3





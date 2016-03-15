-- write yourself a scheme in 48 hours!
--  from https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.List.Split hiding (oneOf,sepBy,endBy)
import Control.Monad
import Numeric
import Data.Complex
import Data.Ratio

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)

numberizeList :: [String] -> [Int]
numberizeList = map (\x -> read x :: Int)

main :: IO()
main = getArgs >>= print . eval . readExpr . head

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseSingleChar :: Parser Char
parseSingleChar = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many $ escapedChars <|> noneOf "\"\\"
	char '"'
	return $ String x

escapedChars :: Parser Char
escapedChars = do 
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
        '\\' -> x
        '"' -> x
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'

parseAtom :: Parser LispVal
parseAtom = do 
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
	let atom = first:rest
	return $ case atom of
		"#t" -> Bool True
		"#f" -> Bool False
		_    -> Atom atom

parseBool :: Parser LispVal
parseBool  = do
    char '#'
    (char 't'  >> return (Bool True)) <|> (char 'f' >> return (Bool False))

radixPrefix :: Parser String
radixPrefix = string "#b" <|> string "#o" <|> string "#d" <|> string "#x"
-- can't we map this or something to make it nicer?

parseNumber :: Parser LispVal
parseNumber = 
    parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do try $ string "#d"
                   x <- many1 digit
                   (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)


hex2dig x = fst . head . readHex $ x
oct2dig x = fst . head . readOct $ x
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

parseNumberOld :: Parser LispVal
parseNumberOld = liftM (Number . read) $ many1 digit

parseNumberUsingDo :: Parser LispVal -- as an exercise
parseNumberUsingDo = do
    x <- many1 digit
    return $ (Number . read) x

parseNumberUsingBind :: Parser LispVal -- as an exercise
parseNumberUsingBind = many1 digit >>= \x ->  return $ (Number . read) x

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (string "newline" <|> string "space")
             <|> do { x <- anyChar; notFollowedBy alphaNum; return [x]}
    return $ Character $ case value of 
        "space" -> ' '
        "newline" -> '\n'
        otherwise -> head value

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst . head $ readFloat (x ++ "." ++ y))

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

parseComplex :: Parser LispVal
parseComplex = do x <- try (parseFloat <|> parseDecimal1)
                  char '+'
                  y <- try (parseFloat <|> parseDecimal1)
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

parseList :: Parser LispVal
parseList = liftM List $ parseExpr `sepBy` spaces

parseDottedList :: Parser LispVal
parseDottedList = do 
    head <- parseExpr `endBy` spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom 
          <|> parseString
          <|> try parseComplex
          <|> try parseFloat
          <|> try parseRatio
          <|> try parseNumber
          <|> try parseBool
          <|> try parseCharacter
          <|> parseQuoted 
          <|> do char '('
                 x <- try parseList <|> parseDottedList
                 char ')'
                 return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
	Left err -> String $ "No match: " ++ show err
	Right val -> val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func:args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+",numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer,String)] in
                         if null parsed
                            then 0
                            else fst . head $ parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0


















-- $(x^2 + 5x).diff(x).add(x).eval(x=5)
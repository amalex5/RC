-- toycalc

expr -> term '+' expr | term
term -> factor '*' term | factor
factor -> digit | '(' expr ')'
digit -> '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'

expr :: Parser Int
expr = do t <- term
          do char '+'
          e <- expr
          return (t + e)
          +++ return t

factor :: Parser Int
factor = do d <- digit
            return (digitToInt d)
            +++ do char '(' 
            	   e <- expr
            	   char ')'
                   return e

eval :: String -> Int
eval xs = fst (head (parse expr xs))
-- symdiff

import System.IO
import Expr
import Parser
import Diff
import Simplify

main :: IO ()
main = runRepl

flushStr :: String -> IO ()
flushStr str = putStr str >> hSetBuffering stdin LineBuffering --hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
--evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)
evalString expr = return $ show (pprint . evalWrapper . parseWrappedExpressions $ expr)

evalAndPrint :: String -> IO ()
evalAndPrint expr =  (evalString expr) >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "sd>>> ") evalAndPrint


first :: [a] -> a
first = head

second :: [a] -> a
second = head . tail

evalWrapper :: [WrapperFxn] -> Expr
evalWrapper = foldl foldingFxn (Val 0)
  where 
    foldingFxn _ (WrapperFxn ("$",y)) = (parseExpr y)
    foldingFxn b (WrapperFxn ("diff",y)) = diff y b
    foldingFxn b (WrapperFxn ("simplify",_)) = simplify b
    foldingFxn b (WrapperFxn ("eval",x)) = evalExpr (parseEquals x) b
    foldingFxn b (WrapperFxn ("add",y)) = (Add b (parseExpr y) )
    foldingFxn b (WrapperFxn ("sub",y)) = (Sub b (parseExpr y) )
    foldingFxn b (WrapperFxn ("mul",y)) = (Mul b (parseExpr y) )
    foldingFxn b (WrapperFxn ("div",y)) = (Div b (parseExpr y) )
    -- deal with errors. 

--evalWrapper (WrapperFxn (a,b)) = case a of
--  "$" -> parseExpr b
--  "diff" -> undefined -- how do we

testWrappers = [testWrapper1,testWrapper2]
testWrapper1 = WrapperFxn ("$","x^7 + sin(5*x)")
testWrapper2 = WrapperFxn ("diff","x")
--- AAAAAGH MAKE IT WORK FOR SPACES!!!!!!
testWrapper3 = WrapperFxn ("add","x^7+sin(5*x)")
testWrapper4 = WrapperFxn ("add","x^5+sin(7*x)")

evalExpr :: (Expr,Expr) -> Expr -> Expr
-- in the expression e, replace all occurances of a with b
evalExpr (a,b) e = if e == a then b else exprMap (evalExpr (a,b)) e

testEvalExpr = evalExpr ((Var "x"),(Val 5))   (Mul (Add (Val 7) (Var "b")) (Div (Val 1) (Var "x"))) 
testEvalExpr2 = evalExpr ((Var "x"),(Var "i've been replaced!"))   (Add (Var "j") (Mul (Val 7) (Fxn "sin" (Var "x"))) )


-- redo this so it's some pretty tree un-parsing with operational priority!
-- in particular, need to do this in a non-hacky way...
pprint :: Expr -> [Char]
pprint (Neg x) = "-" ++ pprint x
pprint (Add x y) = pprint x ++ "+" ++ pprint y
--pprint (Mul Symbol (Val x)) = show x ++ "x"
--pprint (Mul (Val x) Symbol) = show x ++ "x"
pprint (Sub x y) = pprint x ++ "-" ++ pprint y
pprint (Mul x y) = pprint x ++ "*" ++ pprint y
pprint (Div x y) = pprint x ++ "/" ++ pprint y
pprint (Pow x (Val y) ) = pprint x ++ "^" ++ show y
pprint (Pow x y) = pprint x ++ "^(" ++ pprint y ++ ")"
pprint (Var x) = x
pprint (Val x) = show x
pprint (Fxn "exp" x) = "e^(" ++ pprint x ++ ")"
pprint (Fxn xs y) = xs ++ "(" ++ pprint y ++ ")"

test0 :: Expr
test0 = (Add (Pow (Var "x") (Val 2)) (Mul (Fxn "exp" (Var "x")) (Fxn "sin" ( Mul (Val 5) (Var "x")))))

test1 :: Expr
test1 = (Add (Fxn "sin" (Mul (Val 5) (Pow (Var "x") (Val 7)))) (Mul (Val 4) (Neg (Var "x"))) )

test2 :: Expr
test2 = (Neg (Fxn "cos" (Mul (Var "x") (Val 5))))

test3 :: Expr
test3 = (Fxn "f" (Mul (Fxn "exp" (Mul (Val 5) (Var "x"))) (Pow (Var "x") (Val 7))))

testEmAll = map ddx [test0,test1,test2,test3]


--display  :: Expr -> IO ()
--display es  =  do result <- ((pprint . simplify . diff) es)
--                  putStr "result is: "
--                  putStr result
--                  putStr "\n"

--main :: IO ()
--main = do hSetBuffering stdout NoBuffering
--          putStrLn "\nandrew's symbolic differentiator!"
--          putStrLn "-----------------------------\n"
--          putStr "enter an expression! : "
--          ns <- readLn
--          display ns

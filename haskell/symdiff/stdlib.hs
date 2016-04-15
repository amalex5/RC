module StdLib
where

import Expr

getLibFxn :: Expr -> WrapperFunction -> Expr
getLibFxn b x = 
module Scheme.Util where

import Scheme.LISP (cons)
import Scheme.DataType

import DeepControl.Applicative
import DeepControl.Monad

--------------------------------------------------
-- isX
--------------------------------------------------

true  = SYM "#t" Nothing
false = SYM "#f" Nothing
cell :: Expr -> Expr -> Expr 
cell = cons
sym :: Name -> Expr 
sym name = SYM name Nothing

--------------------------------------------------
-- Expr
--------------------------------------------------

showExpr :: Expr -> String
showExpr NIL            = "NIL"
showExpr (INT x)        = "(INT " ++ show x ++ ")"
showExpr (REAL x)       = "(REAL " ++ show x ++ ")"
showExpr (SYM x _)      = "(SYM " ++ show x ++ " Nothing)"
showExpr (STR x)        = "(STR " ++ show x ++ ")"
showExpr (CELL a d _)   = "(CELL " ++ showExpr a ++ " " ++ showExpr d ++ " Nothing)"
showExpr (AFUNC x _)    = "(AFUNC " ++ show x ++ ")"
showExpr (WAFUNC x _)   = "(WAFUNC " ++ show x ++ ")"
showExpr (RFUNC x _)    = "(RFUNC " ++ show x ++ ")"
showExpr (SYNT x _)     = "(SYNT " ++ show x ++ ")"
showExpr (PROC x _)     = "(PROC " ++ show x ++ ")"
showExpr (CLOS cell _)  = "(CLOS " ++ showExpr cell ++ " _)"
showExpr (CLOSM cell _) = "(CLOSM " ++ showExpr cell ++ " _)"
showExpr (REF ref)      = "(REF " ++ show ref ++ ")"
showExpr (THUNK (x,y))  = "(THUNK " ++ show x ++ ")"


-- prety print
showppExpr :: Expr -> String
showppExpr c = iter 1 c
  where
    iter count (CELL a d _) = "(CELL"
                            ++ (if   isCELL a 
                                then " " ++ iter (count+1) a
                                else " " ++ iter (count+1) a)
                            ++ (if   isCELL d 
                                then linebreak ++ space count ++ iter (count+1) d
                                else " " ++ iter (count+1) d) ++ " Nothing)"
      where
        isCELL (CELL _ _ _) = True
        isCELL _            = False
        space count = take (count*7) $ repeat ' '
        linebreak = "\n"
    iter count e          = showExpr e




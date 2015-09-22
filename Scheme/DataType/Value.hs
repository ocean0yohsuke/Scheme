module Scheme.DataType.Value where

import MonadX.Applicative


data Value = NIL
           | INT  Integer 
           | REAL Double 
           | SYM  String 
           | STR  String 
           | CELL Value Value 

instance Show Value where
    show NIL          = "()"
    show (INT x)      = show x
    show (REAL x)     = show x
    show (SYM x)      = x
    show (STR x)      = show x
    show (CELL (SYM "quote") (CELL expr NIL))            = "'" ++ show expr
    show (CELL (SYM "quasiquote") (CELL expr NIL))       = "`" ++ show expr
    show (CELL (SYM "unquote") (CELL expr NIL))          = "," ++ show expr
    show (CELL (SYM "unquote-splicing") (CELL expr NIL)) = ",@" ++ show expr
    show c@(CELL a d) = "(" ++ showCELL c ++ ")"
      where
        showCELL NIL        = ""
        showCELL (CELL a d) = show a ++ case d of
          NIL          -> ""
          c@(CELL _ _) -> " " ++ showCELL c
          e            -> " . " ++ show e


instance Eq Value where
    NIL        == NIL        = True
    INT x      == INT y      = x == y
    REAL x     == REAL y     = x == y
    SYM x      == SYM y      = x == y
    STR x      == STR y      = x == y
    CELL l r   == CELL l' r' = (l,r) == (r,r')
    _          == _          = False


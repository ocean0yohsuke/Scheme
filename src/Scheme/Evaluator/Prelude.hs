module Scheme.Evaluator.Prelude where

import Scheme.Evaluator.Micro
import Scheme.DataType.Error.Eval
import Scheme.LISP as L

import qualified Data.Map as M

--------------------------------------------------
-- GEnv
--------------------------------------------------

preludeGEnv :: GEnv
preludeGEnv = M.fromList [ 
      ("+",      AFUNC "+" evalAdds)
    , ("-",      AFUNC "-" evalSubs)
    , ("*",      AFUNC "*" evalMuls)
    , ("/",      AFUNC "/" evalDivs)
    , ("^",      AFUNC "^" evalPower)
    , ("mod",    AFUNC "mod" evalMod)
    , ("=",      AFUNC "=" evalEqNum)
    , ("<",      AFUNC "<" evalLtNum)
    , (">",      AFUNC ">" evalGtNum)
    , ("<=",     AFUNC "<=" evalLtEq)
    , (">=",     AFUNC ">=" evalGtEq)
    , ("equal?", AFUNC "equal?" evalEqual)
    , ("number?", AFUNC "number?" evalNumber)
    , ("symbol?", AFUNC "symbol?" evalSymbol)
    , ("string?", AFUNC "string?" evalString)

    , ("error", SYNT "error" evalError)
    , ("exit",  PROC "exit" evalExit)
    , ("eval",  SYNT "eval" evalEval)
    , ("evalmacro",  SYNT "evalmacro" evalEvalMacro)
    ]

--------------------------------------------------
-- Prelude
--------------------------------------------------

evalAdds :: AFunc
evalAdds xs = foldlM add (INT 0) xs
  where
    add :: Expr -> AFunc
    add (INT x)  (INT y)  = (*:) $ INT (x + y)
    add (INT x)  (REAL y) = (*:) $ REAL (fromIntegral x + y)
    add (REAL x) (INT y)  = (*:) $ REAL (x + fromIntegral y)
    add (REAL x) (REAL y) = (*:) $ REAL (x + y)
    add x        y        = throwEvalError $ WRONGTypeArg $ show (cell (sym "+") xs) 

evalSubs :: AFunc
evalSubs (CELL (INT a) NIL _)  = (*:) $ INT (-a)
evalSubs (CELL (REAL a) NIL _) = (*:) $ REAL (-a)
evalSubs c@(CELL a rest _)     = foldlM sub a rest
  where
    sub :: Expr -> AFunc
    sub (INT x)  (INT y)  = (*:) $ INT (x - y)
    sub (INT x)  (REAL y) = (*:) $ REAL (fromIntegral x - y)
    sub (REAL x) (INT y)  = (*:) $ REAL (x - fromIntegral y)
    sub (REAL x) (REAL y) = (*:) $ REAL (x - y)
    sub x        y        = throwEvalError $ WRONGTypeArg $ show (cell (sym "-") c) 
evalSubs e = throwEvalError $ INVALIDForm $ show (cell (sym "-") e) 

evalMuls :: AFunc
evalMuls xs = foldlM mul (INT 1) xs
  where
    mul :: Expr -> AFunc
    mul (INT x)  (INT y)  = (*:) $ INT (x * y)
    mul (INT x)  (REAL y) = (*:) $ REAL (fromIntegral x * y)
    mul (REAL x) (INT y)  = (*:) $ REAL (x * fromIntegral y)
    mul (REAL x) (REAL y) = (*:) $ REAL (x * y)
    mul x        y        = throwEvalError $ WRONGTypeArg $ show (cell (sym "*") xs)

evalDivs :: AFunc
evalDivs c@(CELL a x _) = case x of
    NIL  -> div' (INT 1) a
    rest -> foldlM div' a rest
  where
    div' :: Expr -> AFunc
    div' _        (INT 0)  = throwEvalError $ DIVIDEDByZero $ show (cell (sym "/") c) 
    div' _        (REAL 0) = throwEvalError $ DIVIDEDByZero $ show (cell (sym "/") c) 
    div' (INT x)  (INT y)  = (*:) $ INT (x `div` y)
    div' (INT x)  (REAL y) = (*:) $ REAL (fromIntegral x / y)
    div' (REAL x) (INT y)  = (*:) $ REAL (x / fromIntegral y)
    div' (REAL x) (REAL y) = (*:) $ REAL (x / y)
    div' x        y        = throwEvalError $ WRONGTypeArg $ show (cell (sym "/") c) 
evalDivs e = throwEvalError $ INVALIDForm $ show (cell (sym "/") e) 

evalPower :: AFunc
evalPower c@(CELL a (CELL b NIL _) _) = expt a b
  where
    expt :: Expr -> AFunc
    expt (INT x)  (INT y)  = (*:) $ INT (x ^ y)
    expt (REAL x) (INT y)  = (*:) $ REAL (x ^ fromIntegral y)
    expt x        y        = throwEvalError $ WRONGTypeArg $ show (cell (sym "^") c)  
evalPower e = throwEvalError $ INVALIDForm $ show (cell (sym "^") e)  

evalMod :: AFunc
evalMod c@(CELL _ (CELL (INT 0) NIL _) _)     = throwEvalError $ DIVIDEDByZero $ show (cell (sym "mod" ) c)  
evalMod c@(CELL _ (CELL (REAL 0) NIL _) _)    = throwEvalError $ DIVIDEDByZero $ show (cell (sym "mod" ) c)  
evalMod (CELL (INT x) (CELL (INT y) NIL _) _) = (*:) $ INT (mod x y)
evalMod e = throwEvalError $ INVALIDForm $ show (cell (sym "mod" ) e)  

compareNum :: Expr -> Expr -> Scm Ordering
compareNum (INT x)  (INT y)  = (*:) $ compare x y
compareNum (INT x)  (REAL y) = (*:) $ compare (fromIntegral x) y
compareNum (REAL x) (INT y)  = (*:) $ compare x (fromIntegral y)
compareNum (REAL x) (REAL y) = (*:) $ compare x y
compareNum x        y        = throwEvalError $ ILLArgNumReq $ show x_or_y  
  where
    x_or_y = if isNum x
             then y
             else x
      where
        isNum (INT _)  = True
        isNum (REAL _) = True
        isNum _        = False
compareNums :: ((Ordering -> Bool), String) -> AFunc
compareNums (p,_) (CELL x (CELL y NIL _) _)  = do
    r <- compareNum x y
    if p r
    then (*:) true
    else (*:) false
compareNums (p,s) (CELL x ys@(CELL y _ _) _) = do
    r <- compareNum x y
    if p r 
    then compareNums (p,s) ys
    else (*:) false
compareNums (_,s) e = throwEvalError $ INVALIDForm $ show (cell (STR s) e)
evalEqNum, evalLtNum, evalGtNum, evalLtEq, evalGtEq :: AFunc
evalEqNum = compareNums ((== EQ),"=")
evalLtNum = compareNums ((== LT),"<")
evalGtNum = compareNums ((== GT),">")
evalLtEq  = compareNums ((<= EQ),"<=")
evalGtEq  = compareNums ((>= EQ),">=")

-- equal?
evalEqual :: AFunc
evalEqual (CELL x (CELL y NIL _) _) =
    if iter x y 
    then (*:) true 
    else (*:) false
  where 
    iter (CELL a b _) (CELL c d _) = iter a c && iter b d
    iter x            y            = x == y
evalEqual e = throwEvalError $ INVALIDForm $ show (cell (sym "equal?") e)  

-- number?
evalNumber :: AFunc
evalNumber (CELL (INT _)  NIL _) =　(*:) true 
evalNumber (CELL (REAL _) NIL _) = (*:) true 
evalNumber (CELL _        NIL _) = (*:) false 
evalNumber e = throwEvalError $ INVALIDForm $ show (cell (sym "number?") e)  

-- symbol?
evalSymbol :: AFunc
evalSymbol (CELL (SYM _ _) NIL _) =　(*:) true 
evalSymbol (CELL _         NIL _) = (*:) false 
evalSymbol e = throwEvalError $ INVALIDForm $ show (cell (sym "symbol?") e)  

-- string?
evalString :: AFunc
evalString (CELL (STR _) NIL _) =　(*:) true 
evalString (CELL _       NIL _) = (*:) false 
evalString e = throwEvalError $ INVALIDForm $ show (cell (sym "string?") e)  

-------------------------------------------------------------
-- Command
-------------------------------------------------------------

-- error
evalError :: Synt
evalError (CELL (STR x) NIL _)            = throwEvalError $ strMsg $ "ERROR: " ++ x
evalError (CELL (STR x) (CELL y NIL _) _) = do 
    v <- eval y
    throwEvalError $ strMsg $ "ERROR: " ++ x ++ " " ++ show y ++ " <- " ++ show v
evalError (CELL x NIL _)                  = do 
    v <- eval x
    throwEvalError $ strMsg $ "ERROR: " ++ " " ++ show x ++ " as " ++ show v
evalError e = throwEvalError $ INVALIDForm $ show (cell (sym "error") e)  

-- exit
evalExit :: Proc
evalExit NIL = throwScmError EXITErr
evalExit e   = throwEvalError $ INVALIDForm $ show (cell (sym "exit") e)  

-- eval
evalEval :: Synt
evalEval (CELL expr NIL _) = expr >- (actual >=> evalMacro >=> thisEval)
evalEval e = throwEvalError $ INVALIDForm $ show (cell (sym "eval") e)  

-- evalmacro
evalEvalMacro :: Synt
evalEvalMacro (CELL expr NIL _) = expr >- (actual >=> evalMacro) >- (<$|RETURN)
evalEvalMacro e = throwEvalError $ INVALIDForm $ show (cell (sym "evalmacro") e)  


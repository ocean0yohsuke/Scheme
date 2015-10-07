module Scheme.Evaluator.Prelude2 where

import Scheme.Evaluator.Micro
import Scheme.Evaluator.Prelude
import Scheme.DataType.Error.Eval
import Scheme.LISP as L

import qualified Data.Map as M

--------------------------------------------------
-- GEnv
--------------------------------------------------

prelude2GEnv :: GEnv
prelude2GEnv = M.fromList [ 
      ("let*",   SYNT "let*" evalLetStar)
    , ("letrec", SYNT "letrec" evalLetRec)

    , ("append", AFUNC "append" evalAppend)
    , ("length", AFUNC "length" evalLength)

    , ("defined?", AFUNC "defined?" evalDefined)
    ]

--------------------------------------------------
-- letkind
--------------------------------------------------

-- let*
evalLetStar :: Synt
evalLetStar c@(CELL pairs seq _) = do
    let (p,ps) = (L.last pairs, L.init pairs)
        lastlet = cell (sym "let") (cell (cell p nil) seq)
    letform <- L.foldrM toLetForm lastlet ps
    thisEval letform
  where
    toLetForm pair@(CELL (SYM name _) (CELL arg NIL _) _) acc = (*:) $ cell (sym "let") (cell (cell pair nil) (cell acc nil))
    toLetForm e                                           _   = throwEvalError $ strMsg $ "invalid let* form: expected symbol-and-expression pair, but detected "++ show e
evalLetStar e = throwEvalError $ INVALIDForm $ show (cell (sym "let*" ) e)

-- letrec
evalLetRec :: Synt
evalLetRec c@(CELL pairs seq _) = do
    seqM define pairs
    seqM thisEval seq
  where 
    define :: Expr -> Scm ReturnE
    define p@(CELL (SYM name _) (CELL arg NIL _) _) = do
        isDefined <- evalDefined $ cell (STR name) nil
        if isDefined == true
        then evalSet p
        else evalDef p
    define e = throwEvalError $ strMsg $ "invalid letrec form: expected symbol-and-expression pair, but detected "++ show e
evalLetRec e = throwEvalError $ INVALIDForm $ show (cell (sym "letrec" ) e)

--------------------------------------------------
-- AFunc
--------------------------------------------------

-- append
evalAppend :: AFunc
evalAppend c = (*:) $ L.foldl L.append NIL c
  
-- length
evalLength :: AFunc
evalLength c@(CELL xs NIL _) = (*:) $ INT $ toInteger (L.length xs)
evalLength e = throwEvalError $ INVALIDForm $ show (cell (sym "length") e)  

--------------------------------------------------
-- misc
--------------------------------------------------

-- defined?
evalDefined :: AFunc
evalDefined (CELL (STR name) NIL _) = do
    mv <- lookupGEnv name
    case mv of
      Nothing -> (*:) false
      Just _  -> (*:) true
evalDefined e = throwEvalError $ INVALIDForm $ show (cell (sym "defined?") e)  


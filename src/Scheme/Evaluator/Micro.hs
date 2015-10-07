module Scheme.Evaluator.Micro (
    module Scheme.Evaluator.Eval,
    microGEnv,

    evalCar, evalCdr, evalCons, evalPair,
    evalSet, evalSetCar, evalSetCdr,

    evalEq, evalApply,
    evalQuote, evalIf, evalLambda,
    evalDef,
    evalY, evalDefMacro,

) where

import Scheme.Evaluator.Eval
import Scheme.DataType.Error.Eval
import Scheme.LISP as L

import qualified Data.Map as M

-- for debug
import Debug.Trace 

--------------------------------------------------
-- GEnv
--------------------------------------------------

microGEnv :: GEnv
microGEnv = M.fromList [
      ("#t", true)
    , ("#f", false)

    , ("car",    RFUNC "car" evalCar)
    , ("cdr",    RFUNC "cdr" evalCdr)
    , ("cons",   WAFUNC "cons" evalCons)

    , ("list?",  WAFUNC "list?" evalList)
    , ("pair?",  WAFUNC "pair?" evalPair)

    , ("eq?",    AFUNC "eq?" evalEq)

    , ("set!",     SYNT "set!" evalSet)
    , ("set-car!", SYNT "set-car!" evalSetCar)
    , ("set-cdr!", SYNT "set-cdr!" evalSetCdr)

    , ("apply",  SYNT "apply" evalApply)

    , ("quote",  SYNT "quote" evalQuote)
    , ("if",     SYNT "if" evalIf)
    , ("define", SYNT "define" evalDef)
    , ("let",    SYNT "let" evalLet)
    , ("define-macro", SYNT "define-macro" evalDefMacro)
    , ("lambda", SYNT "lambda" evalLambda)
    , ("lambda-macro", SYNT "lambda-macro" evalLambdaMacro)

    -- macro
    , ("quasiquote", SYNT "quasiquote" evalQuasiQuote)
    , ("unquote", SYNT "unquote" evalUnQuote)
    , ("unquote-splicing", SYNT "unquote-splicing" evalUnQuoteSplicing)

    -- combinator
    , ("Y", SYNT "Y" evalY)         -- Y-combinator
    , ("P", SYNT "P" evalP)         -- P-combinator
    , ("Q", SYNT "Q" evalQ)         -- Q-combinator
   ]

----------------------------------------------------------------------------------------------------------------
-- Quasi-Quote
----------------------------------------------------------------------------------------------------------------

-- quasiquote
evalQuasiQuote :: Synt
evalQuasiQuote (CELL macro NIL _) = do
    code <- compileQQ macro
    (*:) $ RETURN code
  where
    compileQQ :: Expr -> Scm Expr
    compileQQ c@(CELL (SYM "unquote" _) x msp) = localMSP msp $ do
      case x of
        CELL expr NIL _ -> weekactual expr
        e -> throwEvalError $ INVALIDForm $ show c
    compileQQ c@(CELL (CELL (SYM "unquote-splicing" _) x msp) d msp') = localMSP msp $ do 
      case x of
        CELL expr NIL _ -> do 
            a <- weekactual expr
            d <- localMSP msp' $ compileQQ d
            if L.isNil d
            then (*:) a
            else if L.isList a && L.isCell d
                 then (*:) $ L.append a d
                 else throwEvalError $ INVALIDForm $ show c
        e -> throwEvalError $ INVALIDForm $ show (cell (sym "unquote-splicing") e)  
    compileQQ c@(CELL (SYM "unquote-splicing" _) x msp) = localMSP msp $ throwEvalError $ INVALIDForm $ show c
    compileQQ c@(CELL (SYM "quasiquote" _) x msp) = localMSP msp $ x >- (evalQuasiQuote >=> catchVoid) -- (*:) c
    compileQQ c@(CELL a d msp) = localMSP msp $ CELL |$> compileQQ a |*> compileQQ d |* msp
    compileQQ e = (*:) e
evalQuasiQuote e = throwEvalError $ INVALIDForm $ show e 

-- unquote
evalUnQuote :: Synt
evalUnQuote (CELL _ _ _) = throwEvalError $ strMsg "unquote appeared outside quasiquote"
evalUnQuote e = throwEvalError $ INVALIDForm $ show e 

-- quasiquote
evalUnQuoteSplicing :: Synt
evalUnQuoteSplicing (CELL _ _ _) = throwEvalError $ strMsg "unquote-splicing appeared outside quasiquote"
evalUnQuoteSplicing e = throwEvalError $ INVALIDForm $ show e 

----------------------------------------------------------------------------------------------------------------
-- Reference Function
----------------------------------------------------------------------------------------------------------------

evalCar :: RFunc
evalCar (CELL (CELL a _ _) NIL _)   = (*:) a
evalCar (CELL r@(REF _)    NIL msp) = do
    x <- r >- unREF
    case x of
      CELL a@(REF _) _  _   -> (*:) a
      CELL a         d  msp -> do
        ref <- newVarRef (Ve a)
        let v = REF ref
        writeREF r (CELL v d msp)
        (*:) v
      _                　　　　-> evalCar (CELL x NIL msp)
evalCar c@(CELL _ NIL _) = throwEvalError $ WRONGTypeArg $ show (cell (sym "car") c) 
evalCar c@(CELL _ _   _) = throwEvalError $ WRONGNumArgs $ show (cell (sym "car") c) 
evalCar e                = throwEvalError $ INVALIDForm $ show (cell (sym "car") e) 

evalCdr :: RFunc
evalCdr (CELL (CELL _ d _) NIL _)   = (*:) d
evalCdr (CELL r@(REF _)    NIL msp) = do
    x <- r >- unREF
    case x of
      CELL _ d@(REF _) _  　-> (*:) d
      CELL a d         msp -> do
        ref <- newVarRef (Ve d)
        let v = REF ref
        writeREF r (CELL a v msp)
        (*:) v
      _                　　　　-> evalCdr (CELL x NIL msp)
evalCdr c@(CELL _ NIL _) = throwEvalError $ WRONGTypeArg $ show (cell (sym "cdr") c)  
evalCdr c@(CELL _ _   _) = throwEvalError $ WRONGNumArgs $ show (cell (sym "cdr") c) 
evalCdr e                = throwEvalError $ INVALIDForm $ show (cell (sym "cdr") e) 

evalCons :: WAFunc
evalCons (CELL a (CELL x NIL _) _) = (*:) $ CELL a x Nothing
evalCons c@(CELL a NIL          _) = throwEvalError $ WRONGNumArgs $ show (cell (sym "cons") c) 
evalCons c@(CELL a (CELL x _ _) _) = throwEvalError $ WRONGNumArgs $ show (cell (sym "cons") c) 
evalCons c@(CELL _ _            _) = throwEvalError $ WRONGTypeArg $ show (cell (sym "cons") c) 
evalCons e                         = throwEvalError $ INVALIDForm $ show (cell (sym "cons") e)  

--
-- Set: These functions work well only on lazy evaluation.
--

-- set! 
evalSet :: Synt
evalSet (CELL x (CELL v NIL _) _) = do
    x <- x >- reference     
    v <- v >- actual
    case x of
      r@(REF _) -> writeREF x v
      _         -> (*:) ()
    (*:) VOID
evalSet e = throwEvalError $ INVALIDForm $ show (cell (sym "set") e)  

-- set-car!
evalSetCar :: Synt
evalSetCar (CELL x e@(CELL _ NIL _) msp) = evalSet (CELL (cell (sym "car") (cell x nil)) e msp)
evalSetCar e = throwEvalError $ INVALIDForm $ show (cell (sym "set-car!") e)  

-- set-cdr!
evalSetCdr :: Synt
evalSetCdr (CELL x e@(CELL _ NIL _) msp) = evalSet (CELL (cell (sym "cdr") (cell x nil)) e msp)
evalSetCdr e = throwEvalError $ INVALIDForm $ show (cell (sym "set-cdr!") e)  

----------------------------------------------------------------------------------------------------------------
-- Week Actual Function
----------------------------------------------------------------------------------------------------------------

-- list?
evalList :: WAFunc
evalList (CELL (CELL _ NIL _) NIL _) =　(*:) true 
evalList (CELL _              NIL _) = (*:) false
evalList e = throwEvalError $ INVALIDForm $ show (cell (sym "list?") e)  

-- pair?
evalPair :: WAFunc
evalPair (CELL (CELL _ _ _) NIL _) = (*:) true
evalPair (CELL _ _ _)              = (*:) false
evalPair e = throwEvalError $ INVALIDForm $ show (cell (sym "pair?") e)  

----------------------------------------------------------------------------------------------------------------
-- Actual Function
----------------------------------------------------------------------------------------------------------------

-- eq?
evalEq :: AFunc
evalEq (CELL x (CELL y NIL _) _) | x == y    = (*:) true 
                                 | otherwise = (*:) false
evalEq e = throwEvalError $ INVALIDForm $ show (cell (sym "eq?") e)  

----------------------------------------------------------------------------------------------------------------
-- Syntax
----------------------------------------------------------------------------------------------------------------

-- apply  
evalApply :: Synt
evalApply (CELL func args@(CELL _ _ _) _) = do
    func' <- actual func
    args' <- L.mapM actual args 
    eagerApply func' (unCELL args')
  where
    unCELL :: Expr -> Expr
    unCELL (CELL x NIL _)  = x
    unCELL (CELL x xs msp) = CELL x (unCELL xs) msp
    unCELL e               = e
evalApply e = throwEvalError $ INVALIDForm $ show (cell (sym "apply") e)  

-- quote
evalQuote :: Synt
evalQuote (CELL expr NIL _) = (*:) $ RETURN expr
evalQuote e = throwEvalError $ INVALIDForm $ show (cell (sym "quote") e)  

-- if
evalIf :: Synt
evalIf (CELL pred (CELL thenForm rest _) _) = do
    v <- actual pred       -- must be actual
    if v /= false          -- (v == true) shall fail.
    then thenForm >- (force >=> thisEval)
    else case rest of
      CELL elseForm _ _ -> elseForm  >- (force >=> thisEval)
      _                 -> (*:) $ RETURN false
evalIf e = throwEvalError $ INVALIDForm $ show (cell (sym "If") e)  

-- define
evalDef :: Synt
-- (define <name> <value>) or
-- (define <func-name> (lambda (<formal-parameters>) <body>))
evalDef (CELL s@(SYM name _) (CELL expr NIL _) _) = ((*:) $ RETURN s) << do
    mlenv <- askMLEnv
    case mlenv of
      Nothing   -> do
        mv <- lookupGEnv name
        case mv of
          Nothing -> do
            v <- thisEval expr >>= catchVoid
            pushGEnv name v
          Just _  -> throwEvalError $ MULTDecl name
      Just lenv -> do
        v <- thisEval expr >>= catchVoid
        pushLEnv name v lenv
-- (define (<func-name> <formal-parameters>) <body>)
evalDef (CELL (CELL s@(SYM _ _) args _) body msp) = do  
    let expr = CELL (SYM "lambda" Nothing) (CELL args body Nothing) Nothing
    evalDef (CELL s (CELL expr NIL Nothing) msp)
evalDef e = throwEvalError $ INVALIDForm $ show (cell (sym "define") e)

-- let
evalLet :: Synt
evalLet c@(CELL pairs seq msp) = do
    (parms, args) <- L.foldrM toTuple (NIL, NIL) pairs
    thisEval $ CELL (CELL (SYM "lambda" Nothing) (CELL parms seq Nothing) Nothing) args msp
  where
    toTuple (CELL parm@(SYM name _) (CELL arg NIL _) _) (params, args) = (*:) (cell parm params, cell arg args)
    toTuple e                                           _              = throwEvalError $ strMsg $ "invalid let form: expected symbol-and-expression pair, but detected "++ show e
evalLet e = throwEvalError $ INVALIDForm $ show (cell (sym "let") e) 

-- define-macro
evalDefMacro :: Synt
evalDefMacro (CELL s@(SYM name _) (CELL l@(CELL lsym@(SYM "lambda" sp') lbody sp) NIL _) _) = ((*:) $ RETURN s) << do
    mv <- lookupGEnv name
    case mv of
      Nothing -> do
        let l' = CELL (sym "lambda-macro") lbody sp
        v <- thisEval l' >>= catchVoid
        pushGEnv name v
      Just _  -> throwEvalError $ MULTDecl name
evalDefMacro e = throwEvalError $ INVALIDForm $ show (cell (sym "define-macro") e) 

-- lambda
evalLambda :: Synt
evalLambda e = do
    mlenv <- askMLEnv
    (*:) $ RETURN $ CLOS e mlenv

-- lambda-macro
evalLambdaMacro :: Synt
evalLambdaMacro e = do
    mlenv <- askMLEnv
    (*:) $ RETURN $ CLOSM e mlenv

-- Y: Y-combinator; for recursion
evalY :: Synt
evalY (CELL x NIL _) = thisEval $ cell y (cell x nil)
  where
    -- (lambda (g)
    --   ((lambda (s) (g (lambda x (apply (s s) x))))
    --    (lambda (s) (g (lambda x (apply (s s) x)))))))
    y = (CELL (SYM "lambda" Nothing)
       (CELL (CELL (SYM "g" Nothing) NIL Nothing)
              (CELL (CELL (CELL (SYM "lambda" Nothing)
                                   (CELL (CELL (SYM "s" Nothing) NIL Nothing)
                                          (CELL (CELL (SYM "g" Nothing)
                                                        (CELL (CELL (SYM "lambda" Nothing)
                                                                      (CELL (SYM "x" Nothing)
                                                                             (CELL (CELL (SYM "apply" Nothing)
                                                                                           (CELL (CELL (SYM "s" Nothing)
                                                                                                         (CELL (SYM "s" Nothing) NIL Nothing) Nothing)
                                                                                                  (CELL (SYM "x" Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) NIL Nothing) Nothing) Nothing)
                            (CELL (CELL (SYM "lambda" Nothing)
                                          (CELL (CELL (SYM "s" Nothing) NIL Nothing)
                                                 (CELL (CELL (SYM "g" Nothing)
                                                               (CELL (CELL (SYM "lambda" Nothing)
                                                                             (CELL (SYM "x" Nothing)
                                                                                    (CELL (CELL (SYM "apply" Nothing)
                                                                                                  (CELL (CELL (SYM "s" Nothing)
                                                                                                                (CELL (SYM "s" Nothing) NIL Nothing) Nothing)
                                                                                                         (CELL (SYM "x" Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) NIL Nothing) Nothing) Nothing)
evalY e = throwEvalError $ INVALIDForm $ show (cell (sym "Y") e)  

-- P: P-combinator; for mutual recursion
evalP :: Synt
evalP (CELL e1 (CELL e2 NIL _) _) = thisEval $ cell p (cell e1 (cell e2 nil))
  where
    -- (lambda (g h)
    --     ((lambda (s t)
    --        (s s t))
    --      (lambda (s t)
    --        (g (lambda x (apply (s s t) x))
    --           (lambda x (apply (t s t) x))))
    --      (lambda (s t)
    --        (h (lambda x (apply (s s t) x))
    --           (lambda x (apply (t s t) x))))))
    p = (CELL (SYM "lambda" Nothing)
       (CELL (CELL (SYM "g" Nothing)
                     (CELL (SYM "h" Nothing) NIL Nothing) Nothing)
              (CELL (CELL (CELL (SYM "lambda" Nothing)
                                   (CELL (CELL (SYM "s" Nothing)
                                                 (CELL (SYM "t" Nothing) NIL Nothing) Nothing)
                                          (CELL (CELL (SYM "s" Nothing)
                                                        (CELL (SYM "s" Nothing)
                                                               (CELL (SYM "t" Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing)
                            (CELL (CELL (SYM "lambda" Nothing)
                                          (CELL (CELL (SYM "s" Nothing)
                                                        (CELL (SYM "t" Nothing) NIL Nothing) Nothing)
                                                 (CELL (CELL (SYM "g" Nothing)
                                                               (CELL (CELL (SYM "lambda" Nothing)
                                                                             (CELL (SYM "x" Nothing)
                                                                                    (CELL (CELL (SYM "apply" Nothing)
                                                                                                  (CELL (CELL (SYM "s" Nothing)
                                                                                                                (CELL (SYM "s" Nothing)
                                                                                                                       (CELL (SYM "t" Nothing) NIL Nothing) Nothing) Nothing)
                                                                                                         (CELL (SYM "x" Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing)
                                                                      (CELL (CELL (SYM "lambda" Nothing)
                                                                                    (CELL (SYM "x" Nothing)
                                                                                           (CELL (CELL (SYM "apply" Nothing)
                                                                                                         (CELL (CELL (SYM "t" Nothing)
                                                                                                                       (CELL (SYM "s" Nothing)
                                                                                                                              (CELL (SYM "t" Nothing) NIL Nothing) Nothing) Nothing)
                                                                                                                (CELL (SYM "x" Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing)
                                   (CELL (CELL (SYM "lambda" Nothing)
                                                 (CELL (CELL (SYM "s" Nothing)
                                                               (CELL (SYM "t" Nothing) NIL Nothing) Nothing)
                                                        (CELL (CELL (SYM "h" Nothing)
                                                                      (CELL (CELL (SYM "lambda" Nothing)
                                                                                    (CELL (SYM "x" Nothing)
                                                                                           (CELL (CELL (SYM "apply" Nothing)
                                                                                                         (CELL (CELL (SYM "s" Nothing)
                                                                                                                       (CELL (SYM "s" Nothing)
                                                                                                                              (CELL (SYM "t" Nothing) NIL Nothing) Nothing) Nothing)
                                                                                                                (CELL (SYM "x" Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing)
                                                                             (CELL (CELL (SYM "lambda" Nothing)
                                                                                           (CELL (SYM "x" Nothing)
                                                                                                  (CELL (CELL (SYM "apply" Nothing)
                                                                                                                (CELL (CELL (SYM "t" Nothing)
                                                                                                                              (CELL (SYM "s" Nothing)
                                                                                                                                     (CELL (SYM "t" Nothing) NIL Nothing) Nothing) Nothing)
                                                                                                                       (CELL (SYM "x" Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing)
evalP e = throwEvalError $ INVALIDForm $ show (cell (sym "P") e)  

-- Q: Q-combinator; for mutual recursion
evalQ :: Synt
evalQ (CELL e1 (CELL e2 NIL _) _) = thisEval $ cell q (cell e1 (cell e2 nil))
  where
    -- (lambda (g h)
    --   ((lambda (s t)
    --      (t s t))
    --    (lambda (s t)
    --      (g (lambda x (apply (s s t) x))
    --         (lambda x (apply (t s t) x))))
    --    (lambda (s t)
    --      (h (lambda x (apply (s s t) x))
    --         (lambda x (apply (t s t) x))))))
    q = (CELL (SYM "lambda" Nothing)
       (CELL (CELL (SYM "g" Nothing)
                     (CELL (SYM "h" Nothing) NIL Nothing) Nothing)
              (CELL (CELL (CELL (SYM "lambda" Nothing)
                                   (CELL (CELL (SYM "s" Nothing)
                                                 (CELL (SYM "t" Nothing) NIL Nothing) Nothing)
                                          (CELL (CELL (SYM "t" Nothing)
                                                        (CELL (SYM "s" Nothing)
                                                               (CELL (SYM "t" Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing)
                            (CELL (CELL (SYM "lambda" Nothing)
                                          (CELL (CELL (SYM "s" Nothing)
                                                        (CELL (SYM "t" Nothing) NIL Nothing) Nothing)
                                                 (CELL (CELL (SYM "g" Nothing)
                                                               (CELL (CELL (SYM "lambda" Nothing)
                                                                             (CELL (SYM "x" Nothing)
                                                                                    (CELL (CELL (SYM "apply" Nothing)
                                                                                                  (CELL (CELL (SYM "s" Nothing)
                                                                                                                (CELL (SYM "s" Nothing)
                                                                                                                       (CELL (SYM "t" Nothing) NIL Nothing) Nothing) Nothing)
                                                                                                         (CELL (SYM "x" Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing)
                                                                      (CELL (CELL (SYM "lambda" Nothing)
                                                                                    (CELL (SYM "x" Nothing)
                                                                                           (CELL (CELL (SYM "apply" Nothing)
                                                                                                         (CELL (CELL (SYM "t" Nothing)
                                                                                                                       (CELL (SYM "s" Nothing)
                                                                                                                              (CELL (SYM "t" Nothing) NIL Nothing) Nothing) Nothing)
                                                                                                                (CELL (SYM "x" Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing)
                                   (CELL (CELL (SYM "lambda" Nothing)
                                                 (CELL (CELL (SYM "s" Nothing)
                                                               (CELL (SYM "t" Nothing) NIL Nothing) Nothing)
                                                        (CELL (CELL (SYM "h" Nothing)
                                                                      (CELL (CELL (SYM "lambda" Nothing)
                                                                                    (CELL (SYM "x" Nothing)
                                                                                           (CELL (CELL (SYM "apply" Nothing)
                                                                                                         (CELL (CELL (SYM "s" Nothing)
                                                                                                                       (CELL (SYM "s" Nothing)
                                                                                                                              (CELL (SYM "t" Nothing) NIL Nothing) Nothing) Nothing)
                                                                                                                (CELL (SYM "x" Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing)
                                                                             (CELL (CELL (SYM "lambda" Nothing)
                                                                                           (CELL (SYM "x" Nothing)
                                                                                                  (CELL (CELL (SYM "apply" Nothing)
                                                                                                                (CELL (CELL (SYM "t" Nothing)
                                                                                                                              (CELL (SYM "s" Nothing)
                                                                                                                                     (CELL (SYM "t" Nothing) NIL Nothing) Nothing) Nothing)
                                                                                                                       (CELL (SYM "x" Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing) NIL Nothing) Nothing) Nothing)

evalQ e = throwEvalError $ INVALIDForm $ show (cell (sym "Q") e)  


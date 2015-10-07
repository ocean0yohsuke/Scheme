module Scheme.Evaluator.Eval (
    module Config,
    module Scheme.DataType,
    module Scheme.DataType.Error,
    module Scheme.Action,
    module Scheme.Util,
    module DeepControl.Applicative,
    module DeepControl.Monad,
    module MonadX.Monad.RWS,
    module MonadX.Monad.Error,
    module MonadX.Monad.Reference,

    eval, evalMacro,
    actual, reference, force, weekforce, weekactual, unREF,

    thisEval, thisApply,
    eagerEval, eagerApply,
    lazyEval, lazyApply,

    ) where

import Scheme.LISP as L

import Config
import Scheme.DataType
import Scheme.DataType.Error
import Scheme.DataType.Error.Eval
import Scheme.DataType.Error.Try
import Scheme.Action
import Scheme.Util
import DeepControl.Applicative
import DeepControl.Monad hiding (void)
import MonadX.Monad.RWS hiding (liftCatch)
import MonadX.Monad.Error hiding (liftCatch)
import MonadX.Monad.Reference hiding (liftCatch)

import qualified Data.Map as M
import Data.List 

-- for debug
import Debug.Trace

thisEval :: Expr -> Scm ReturnE 
thisEval x = do
    config <- askConfig
    if isLazyMode config 
    then lazyEval x
    else eagerEval x
thisApply :: Expr -> Expr -> Scm ReturnE
thisApply x y = do
    config <- askConfig
    if isLazyMode config 
    then lazyApply x y
    else eagerApply x y

--------------------------------------------------
-- eval
--------------------------------------------------

eval :: Expr -> Scm ReturnE
eval e = do
    return <- thisEval e
    case return of
      VOID     -> (*:) VOID
      RETURN e -> do
        x <- force e
        -- traceM $ show x
        (*:) $ RETURN x

------------------------------------------------------
-- eval
------------------------------------------------------

force :: Expr -> Scm Expr
force r@(REF _)       = r >- (unREF >=> force)
force (CELL x xs msp) = CELL |$> force x |*> force xs |* msp
force e               = (*:) e

weekforce :: Expr -> Scm Expr
weekforce r@(REF _) = r >- (unREF >=> weekforce)
weekforce e         = (*:) e

actual :: Expr -> Scm Expr
actual r@(REF _) = r >- (force >=> actual)
actual e         = e >- (thisEval >=> catchVoid >=> force)

weekactual :: Expr -> Scm Expr
weekactual r@(REF _) = r >- (weekforce >=> weekactual)
weekactual e         = e >- (thisEval >=> catchVoid >=> weekforce)

reference :: Expr -> Scm Expr
-- reference r@(REF _) = r >- unREF
reference r@(REF _) = r >- delay
reference e         = e >- (thisEval >=> catchVoid)

unREF :: Expr -> Scm Expr
unREF (REF ref) = do
    v <- readVarRef ref
    case v of
      Ve t@(THUNK _) -> deThunk t ref
      Ve e           -> (*:) e　
  where
    deThunk :: Expr -> Ref Var -> Scm Expr
    deThunk (THUNK (r@(REF _), env)) ref = do
        v <- localScm (const env) $ r >- unREF
        memoize v
        (*:) v
    deThunk (THUNK (e, env)) ref = do
        v <- localScm (const env) $ e >- (thisEval >=> catchVoid)
        memoize v
        (*:) v
    memoize :: Expr -> Scm ()
    memoize v = writeVarRef ref (Ve v)
unREF v = (*:) v

delay :: Expr -> Scm Expr
delay x
    | isActual x = (*:) x
    | otherwise  = case x of
        CELL (SYM name _) _ _ -> do
            config <- askConfig
            if name `elem` strictlyEvalFunc config
            then actual x
            else thunk x
        _                     -> thunk x
  where
    thunk x = do
        env <- askScmEnv
        ref <- newVarRef $ Ve (THUNK (x, env))
        (*:) $ REF ref
    isActual :: Expr -> Bool
    isActual NIL                = True
    isActual (INT _)            = True
    isActual (REAL _)           = True
    isActual (STR _)            = True
    isActual (SYM "#t" _)       = True
    isActual (SYM "#f" _)       = True
    isActual (CELL head tail _) = isActual head && isActual tail
    isActual _                  = False


----------------------------------------------------------------------------------------------------------
-- eager evaluation 
----------------------------------------------------------------------------------------------------------

eagerEval :: Expr -> Scm ReturnE
eagerEval NIL              = (*:) $ RETURN NIL
eagerEval v@(INT _)        = (*:) $ RETURN v
eagerEval v@(REAL _)       = (*:) $ RETURN v
eagerEval v@(STR _)        = (*:) $ RETURN v
eagerEval s@(SYM name msp) = localMSP msp $ RETURN |$> do
    mlenv <- askMLEnv
    case mlenv of
      Nothing   -> lookupGEnv'
      Just lenv -> do
        mv <- lookupLEnv name lenv
        case mv of
          Nothing -> lookupGEnv'
          Just v  -> (*:) v
  where
    lookupGEnv' = do
        mv <- lookupGEnv name
        case mv of
          Nothing -> throwEvalError $ UNBOUNDVar name
          Just v  -> (*:) v     
eagerEval c@(CELL head tail msp) = localMSP msp $ do
    x <- actual head
    case x of
      AFUNC _ _    -> tail >- (L.mapM actual >=> eagerApply x)
      WAFUNC _ _   -> tail >- (L.mapM weekactual >=> eagerApply x)
      RFUNC _ _    -> tail >- (L.mapM reference >=> eagerApply x)
      PROC _ _     -> tail >- (L.mapM actual >=> eagerApply x)
      SYNT _ synt  -> synt tail
      CLOS _ _     -> logTrace c >> tryloop head >> do
                      x <- tail >- (L.mapM reference >=> eagerApply x)
                      logTraceR x
                      (*:) x
      CLOSM _ _    -> logTrace c >> do
                      x <- tail >- (eagerApply x >=> catchVoid >=> eagerEval)
                      logTraceR x
                      (*:) x
      _            -> throwEvalError $ strMsg $ "invalid eagerEval application: " ++ show (cell x tail)
eagerEval e = throwEvalError $ strMsg $ "invalid eagerEval form: " ++ show e

eagerApply :: Expr -> Expr -> Scm ReturnE
eagerApply (AFUNC _ f)  args = args >- f >- (<$|RETURN)
eagerApply (WAFUNC _ f) args = args >- f >- (<$|RETURN)
eagerApply (RFUNC _ f)  args = args >- f >- (<$|RETURN)
eagerApply (PROC _ p)   args = args >- p
eagerApply (CLOS (CELL params seq msp) mlenv) args = localMSP msp $ do
    lenv <- newLEnv mlenv
    params <- bind lenv params args
    case params of
      NIL -> localMLEnv (Just lenv) $ seqM eval seq
      _   -> (*:) $ RETURN $ CLOS (CELL params seq msp) (Just lenv)
eagerApply (CLOSM (CELL params seq msp) mlenv) args = localMSP msp $ do
    lenv <- newLEnv mlenv
    params <- bind lenv params args
    case params of
      NIL -> localMLEnv (Just lenv) $ seqM eval seq
      _   -> (*:) $ RETURN $ CLOSM (CELL params seq msp) (Just lenv)
eagerApply func args = throwEvalError $ strMsg $ "invalid eagerApply form: " ++ show (cell func args)

---------------------------------------------------------------------------------------------------
-- lazy evaluation
---------------------------------------------------------------------------------------------------

lazyEval :: Expr -> Scm ReturnE
lazyEval v@NIL                  = eagerEval v
lazyEval v@(INT _)              = eagerEval v
lazyEval v@(REAL _)             = eagerEval v
lazyEval v@(STR _)              = eagerEval v
lazyEval v@(SYM _ _)            = eagerEval v
lazyEval c@(CELL head tail msp) = localMSP msp $ do
    x <- actual head
    case x of
      AFUNC _ _    -> lazyApply x tail
      WAFUNC _ _   -> lazyApply x tail
      RFUNC _ _    -> lazyApply x tail
      PROC _ _     -> lazyApply x tail
      SYNT _ synt  -> synt tail
      CLOS _ _     -> logTrace c >> tryloop head >> do
                      x <- lazyApply x tail
                      logTraceR x
                      (*:) x
      CLOSM _ _    -> logTrace c >> do
                      x <- tail >- (eagerApply x >=> catchVoid >=> lazyEval)
                      logTraceR x
                      (*:) x
      _            -> throwEvalError $ strMsg $ "invalid lazyEval application: " ++ show (cell x tail)
lazyEval e = throwEvalError $ strMsg $ "invalid lazyEval form: " ++ show e

lazyApply :: Expr -> Expr -> Scm ReturnE
lazyApply (AFUNC _ f)  args = args >- (L.mapM actual >=> f) >- (<$|RETURN)
-- for infinite list
lazyApply (WAFUNC "cons" _) c@(CELL a d@(CELL x NIL _) msp) = localMSP msp $ do
    env <- askScmEnv
    a <- newVarRef $ Ve $ THUNK (a, env)
    d <- newVarRef $ Ve $ THUNK (x, env)
    (*:) $ RETURN $ CELL (REF a) (REF d) msp
lazyApply (WAFUNC _ f) args = args >- (L.mapM weekactual >=> f) >- (<$|RETURN)
lazyApply (RFUNC _ f)  args = args >- (L.mapM reference >=> f) >- (<$|RETURN)
lazyApply (PROC _ p)   args = args >- (L.mapM actual >=> p)
lazyApply c@(CLOS (CELL params seq msp) mlenv) args = localMSP msp $ do
    args <- L.mapM delay args
    lenv <- newLEnv mlenv
    params <- bind lenv params args
    case params of
      NIL -> localMLEnv (Just lenv) $ seqM lazyEval seq
      _   -> (*:) $ RETURN $ CLOS (CELL params seq msp) (Just lenv)
lazyApply func args = throwEvalError $ strMsg $ "invalid lazyEval form: " ++ show (cell func args)

---------------------------------------------------------------------------------------------------
-- evalMacro
---------------------------------------------------------------------------------------------------

evalMacro :: Expr -> Scm Expr
evalMacro e = traverse [] e
  where
    traverse :: [Name] -> Expr -> Scm Expr
    traverse binds c@(CELL (SYM "quasiquote" _) _ _) = (*:) c
    traverse binds c@(CELL (SYM "quote" _) _ _) = (*:) c
    traverse binds c@(CELL a@(SYM "define" _)       (CELL _ _ _) _) = traverse_defkind binds c
    traverse binds c@(CELL a@(SYM "define-macro" _) (CELL _ _ _) _) = traverse_defkind binds c
    traverse binds c@(CELL a@(SYM "let" _)    (CELL _ _ _) _) = traverse_letkind binds c
    traverse binds c@(CELL a@(SYM "let*" _)   (CELL _ _ _) _) = traverse_letkind binds c
    traverse binds c@(CELL a@(SYM "letrec" _) (CELL _ _ _) _) = traverse_letkind binds c
    traverse binds c@(CELL sym@(SYM "lambda" _) (CELL params seq msp') msp) = localMSP msp $ do
        let newbinds = L.reduce toName params
        seq <- traverse (newbinds++binds) seq
        (*:) $ CELL sym (CELL params seq msp') msp
    traverse binds c@(CELL head@(SYM name _) tail msp) = localMSP msp $ do
        case elemIndex name binds of
          Just _  -> CELL head |$> traverse binds tail |* msp
          Nothing -> do
            mv <- lookupGEnv name 
            case mv of
              Just ref -> do
                v <- force ref
                case v of
                  CLOSM _ _ -> tail >- (traverse binds >=> eagerApply v >=> catchVoid >=> traverse binds)
                  _         -> CELL head |$> traverse binds tail |* msp
              _      -> CELL head |$> traverse binds tail |* msp
    traverse binds (CELL a d sp) = CELL |$> traverse binds a |*> traverse binds d |* sp
    traverse binds e = (*:) e

    -- traverse letkind
    traverse_letkind binds (CELL a (CELL pairs body msp') msp) = localMSP msp $ do
        (parms, args) <- L.foldrM toTuple (NIL, NIL) pairs
        args <- traverse binds args 
        let newbinds = L.reduce toName parms
        body <- traverse (newbinds++binds) body
        let pairs' = L.zip parms args
        (*:) $ CELL a (CELL pairs' body msp') msp
      where
        toTuple (CELL parm@(SYM name _) (CELL arg NIL _) _) (params, args) = (*:) (cell parm params, cell arg args)
        toTuple e                                           _              = throwEvalError $ strMsg $ "invalid let-kind form: expected symbol-and-expression pair, but detected "++ show e
    traverse_letkind binds e = throwEvalError $ INVALIDForm $ show e

    -- traverse defkind
    traverse_defkind binds (CELL a (CELL s@(SYM name _) (CELL expr NIL msp'') msp') msp) = localMSP msp $ do
        expr <- traverse (name:binds) expr
        (*:) $ CELL a (CELL s (CELL expr NIL msp'') msp') msp
    traverse_defkind binds (CELL a (CELL def@(CELL (SYM name _) params _) body msp') msp)= do
        let newbinds = L.reduce toName params
        body <- traverse ((name:newbinds)++binds) body
        (*:) $ CELL a (CELL def body msp') msp
    traverse_defkind binds e = throwEvalError $ INVALIDForm $ show e

    -- common 
    toName :: Expr -> [Name]
    toName (SYM name _) = [name]
    toName _            = []

---------------------------------------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------------------------------------

-- lambda-binding
bind :: LEnv -> Expr -> Expr -> Scm Expr
bind lenv NIL                         NIL               = (*:) NIL
bind lenv NIL                         e                 = throwEvalError $ BINDErr $ "extra number of argument(s) detected: "++ show e
bind lenv (SYM name _)                arg               = do
    pushLEnv name arg lenv 
    (*:) NIL
bind lenv params@(CELL (SYM _ _) _ _) NIL               = (*:) params
bind lenv (CELL s@(SYM _ _) params _) (CELL arg args _) = do
    bind lenv s arg 
    bind lenv params args
bind _    x                            _                = throwEvalError $ BINDErr $ "invalid parameter: "++ show x

-- This function is for the 'try', a Chaitin function,
-- counting the depth limit only on 'try-loop' function.
tryloop :: Expr -> Scm ()
tryloop (SYM "try-loop" _) = do
    th <- getTryHeap
    case th of
      [] -> (*:) ()
      _  -> do
        if or $ th <$| (\try -> depthLimit try <= DEPTHLIMIT (loopCount try))
        then throwTryError OUTOFTIME
        else putTryHeap $ th <$| (\try -> setLoopCount (loopCount try + 1) try)
tryloop _ = (*:) ()



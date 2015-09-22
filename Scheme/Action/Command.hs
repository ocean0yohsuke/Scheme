module Scheme.Action.Command where

import Scheme.DataType.Misc
import Scheme.DataType
import Scheme.DataType.Error
import Scheme.DataType.Error.Eval
import Scheme.DataType.Error.Try (TryError)
import Scheme.Util

import MonadX.Applicative
import MonadX.Monad
import MonadX.Monad.Error -- hiding (liftCatch)
import MonadX.Monad.Reference
import MonadX.Monad.RWS

-- for debug
import Debug.Trace

--------------------------------------------------
-- Monad Transfer  
--------------------------------------------------

--
-- MonadReference
--
getScmRef :: Scm ScmRef
getScmRef = trans $ get

--
-- MonadReader
--
askScmEnv :: Scm ScmEnv
askScmEnv = trans . trans $ ask
localScm :: (ScmEnv -> ScmEnv) -> Scm a -> Scm a 
localScm f = mapErrorT . mapReferenceT $ local f  -- TODO: 

--
-- MonadState
--
getScmStates :: Scm ScmStates
getScmStates = trans . trans $ get
putScmStates :: ScmStates -> Scm ()
putScmStates = trans . trans . put
stateScmStates :: (ScmStates -> (a, ScmStates)) -> Scm a
stateScmStates = trans . trans . state

--
-- MonadError
--
throwScmError :: ScmError -> Scm a
throwScmError = throwError
catchScmError :: Scm a -> (ScmError -> Scm a) -> Scm a
catchScmError = catchError

--
-- MonadReference
--
newVarRef :: Var -> Scm (Ref Var)
newVarRef v       = trans $ newRef v
readVarRef :: Ref Var -> Scm Var
readVarRef ref    = trans $ readRef ref
writeVarRef :: Ref Var -> Var -> Scm ()
writeVarRef ref v = trans $ writeRef ref v

getRefEnv :: Scm ScmRef
getRefEnv = trans $ get
putRefEnv :: ScmRef -> Scm ()
putRefEnv = trans . put

--------------------------------------------------
-- Error
--------------------------------------------------

throwEvalError :: EvalError -> Scm a
throwEvalError err = do
    msp <- askMSP
    throwScmError $ EVALErr msp err
throwTryError :: TryError -> Scm a
throwTryError err = do
    throwScmError $ TRYErr err

catchVoid :: Return a -> Scm a
catchVoid VOID       = throwEvalError VOIDErr
catchVoid (RETURN e) = (*:) e
{-
catchVoid_ :: Eval -> Scm Expr
catchVoid_ VOID       = (*:) $ STR "<void>"
catchVoid_ (RETURN e) = (*:) e
-}

--------------------------------------------------
-- Reference
--------------------------------------------------

writeREF :: Expr -> Expr -> Scm ()
writeREF r@(REF ref) v = do
    if r == v
    then (*:) ()
    else writeVarRef ref (Ve v)
writeREF e           _ = throwEvalError $ strMsg $ "invalid writeREF form: "++ show e

--------------------------------------------------
-- Env
--------------------------------------------------

askMLEnv :: Scm MLEnv
askMLEnv = do
    (mlenv, _) <- askScmEnv
    (*:) mlenv

askConfig :: Scm Config
askConfig = do
    (_, metainfo) <- askScmEnv
    (*:) $ config metainfo

askMSP :: Scm MSP
askMSP = do
    (_, metainfo) <- askScmEnv
    (*:) $ msp metainfo

localMLEnv :: MLEnv -> Scm a -> Scm a
localMLEnv mlenv = localScm (\(_, b) -> (mlenv, b))

localConfig :: Config -> Scm a -> Scm a
localConfig config = localScm (\(a, metainfo) -> (a, setConfig config metainfo))

localMSP :: MSP -> Scm a -> Scm a
localMSP Nothing x = x
localMSP msp     x = localScm (\(mlenv, metainfo) -> (mlenv, setMSP msp metainfo)) x

--------------------------------------------------
-- ScmStates
--------------------------------------------------

--
-- GEnv
--
getGEnv :: Scm GEnv
getGEnv = do
    (genv, metadata) <- getScmStates
    (*:) genv
putGEnv :: GEnv -> Scm ()
putGEnv genv = do
    metadata <- getMetaData
    putScmStates (genv, metadata)
stateGEnv :: (GEnv -> (a, GEnv)) -> Scm a
stateGEnv f = do
    s <- getGEnv
    let (a, s') = f s
    putGEnv s'
    (*:) a

--
-- MetaData
--
getMetaData :: Scm MetaData
getMetaData = do
    (genv, metadata) <- getScmStates
    (*:) metadata
putMetaData :: MetaData -> Scm ()
putMetaData x = do
    genv <- getGEnv
    putScmStates (genv, x)

--
-- StackTrace
--
getStackTrace :: Scm StackTrace
getStackTrace = do
    metadata <- getMetaData
    (*:) $ stackTrace metadata
putStackTrace :: StackTrace -> Scm ()
putStackTrace st = do
    metadata <- getMetaData
    putMetaData $ setStackTrace st metadata

--
-- Chaitin's Try
--
getTryHeap :: Scm [Try]
getTryHeap = do
    metadata <- getMetaData
    (*:) $ tryHeap metadata
putTryHeap :: [Try] -> Scm ()
putTryHeap th = do
    metadata <- getMetaData
    putMetaData $ setTryHeap th metadata

---------------------------------------------------------------
-- TraceStack
---------------------------------------------------------------

--
-- Trace
--
popTrace :: Scm (Maybe Trace)
popTrace = do
    st <- getStackTrace
    let heap = traceHeap st
    case heap of
      [] -> (*:) Nothing 
      _  -> do
        putStackTrace $ setTraceHeap (tail heap) st
        (*:) $ Just $ head heap
pushTrace :: Trace -> Scm ()
pushTrace x = do
    st <- getStackTrace
    let heap = traceHeap st
    putStackTrace $ setTraceHeap (x:heap) st
logTrace :: Expr -> Scm ()
logTrace expr = do
    msp <- askMSP
    refenv <- getRefEnv
    pushTrace (show expr, msp, Nothing)

--
-- Trace Result
--
popTraceR :: Scm (Maybe TraceR)
popTraceR = do
    st <- getStackTrace
    let heap = traceRHeap st 
    case heap of
      [] -> (*:) Nothing 
      _  -> do
        putStackTrace $ setTraceRHeap (tail heap) st
        (*:) $ Just $ head heap
pushTraceR :: TraceR -> Scm ()
pushTraceR x = do
    st <- getStackTrace
    let heap = traceRHeap st 
    putStackTrace $ setTraceRHeap (x:heap) st
logTraceR :: ReturnE -> Scm ()
logTraceR result = do
    mv <- popTrace
    case mv of
      Just (expr, msp, _) -> do
        case result of
          VOID     -> pushTraceR (expr, msp, Just $ "- void -")
          RETURN v -> pushTraceR (expr, msp, Just $ show v)
      Nothing -> throwScmError $ strMsg $ "logStackTraceResult: stack is empty."

refreshStackTrace :: ScmStates -> ScmStates
refreshStackTrace (genv, metadata) = 
    let st = stackTrace metadata
        newst = st >- (setTraceHeap [] >-> setTraceRHeap [])
    in  (genv, setStackTrace newst metadata)

---------------------------------------------------------------
-- Chaitin's Try
---------------------------------------------------------------

getTry :: Scm (Maybe Try)
getTry = do
    heap <- getTryHeap
    case heap of
      [] -> (*:) Nothing
      _  -> (*:) $ Just $ head heap
pushTry :: Try -> Scm ()
pushTry try = do
    heap <- getTryHeap
    putTryHeap $ try : heap
popTry :: Scm (Maybe Try)
popTry = do
    heap <- getTryHeap
    case heap of
      [] -> (*:) Nothing 
      _ -> do
        putTryHeap $ tail heap
        (*:) $ Just $ head heap

-- TODO: chaitinCapture
capture :: Expr -> Scm ()
capture v = do
    mtry <- popTry
    case mtry of
      Nothing  -> (*:) ()
      Just try -> do
        let cd = capturedDisplays try
        pushTry $ setCapturedDisplays (cd ++ [v]) try


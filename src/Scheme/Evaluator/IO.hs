module Scheme.Evaluator.IO where

import Scheme.Parser (readSchemeCode)
import Scheme.Evaluator.Micro
import Scheme.DataType.Error.Eval
import Scheme.LISP as L

import qualified Prelude as P (foldr, foldl, length, mapM, zip)
import Prelude hiding (foldr, foldl, length, mapM, zip)

import Data.List
import qualified Data.Map as M
import System.IO
import System.FilePath ((</>))

--------------------------------------------------
-- GEnv
--------------------------------------------------

ioGEnv :: GEnv
ioGEnv = M.fromList [ 
      ("load",    SYNT "load" evalLoad)

    , ("newline", PROC "newline" evalNewline)
    , ("display", PROC "display" evalDisplay)
    , ("print",   AFUNC "print" evalPrint)
    , ("read",    PROC "read" evalRead)

    , ("printC", AFUNC "printC" evalPrintC)

    , ("stacktrace",       SYNT "stacktrace" evalStackTrace)
    , ("stacktracelength", SYNT "stacktracelength" evalStackTraceLength)
　   , ("scheme-env",       PROC "scheme-env" evalSchemeEnv)

    , ("show",     PROC "show" evalShow)
    , ("showExpr", SYNT "showExpr" evalShowExpr)

    , ("unittest", SYNT "stacktrace" evalUnitTest)
     ]

-------------------------------------------------------------
-- Procedure
-------------------------------------------------------------

-- load
evalLoad :: Synt
evalLoad (CELL (STR filename) NIL _) = do
    config <- askConfig
    mmv <- liftIO $ loadSchemeCodes (startDir config </> filename)
    case mmv of
      Left err -> throwScmError $ strMsg $ show err
      Right codes -> do
        ccodes <- codes >- P.mapM evalMacro
        mapM_ thisEval ccodes
        (*:) $ RETURN $ STR $ "load: "++ filename
evalLoad e = throwEvalError $ INVALIDForm $ show (cell (sym "load") e) 

-- newline
evalNewline :: Proc
evalNewline NIL = (display "") >> (*:) VOID
evalNewline e = throwEvalError $ INVALIDForm $ show (cell (sym "newline") e) 

-- display
evalDisplay :: Proc
evalDisplay (CELL (STR x) NIL _) = (*:) VOID << (liftIO $ putStr x)
evalDisplay (CELL expr NIL _)    = (*:) VOID << (liftIO $ putStr $ show expr)
evalDisplay e = throwEvalError $ INVALIDForm $ show (cell (sym "display") e) 

-- print
evalPrint :: AFunc
evalPrint (CELL (STR x) NIL _) = do
    display x
    (*:) $ STR x
evalPrint (CELL expr NIL _)    = do
    display $ show expr
    (*:) expr
evalPrint e = throwEvalError $ INVALIDForm $ show (cell (sym "print") e) 

-------------------------------------------------------------
-- Command
-------------------------------------------------------------

-- read
evalRead :: Proc
evalRead NIL = do
    line <- liftIO $ getLine
    read line
  where
    read :: String -> Scm ReturnE
    read [] = throwEvalError $ INVALIDForm $ show (cell (sym "read-scheme") NIL)
    read xs = case readSchemeCode xs of
      Left error              -> (*:) $ RETURN $ STR (show error)
      Right (COMMENT s, _)    -> evalRead NIL
      Right (LINEBREAK, _)    -> evalRead NIL
      Right (EXPR expr, rest) -> (*:) $ RETURN expr
      Right (EOF, _)          -> evalRead NIL
evalRead e = throwEvalError $ INVALIDForm $ show (cell (sym "read-scheme") e)  

-------------------------------------------------------------
-- Chaitin
-------------------------------------------------------------

-- printC: print & capture
evalPrintC :: AFunc
evalPrintC (CELL (STR x) NIL _) = do
    display x
    capture $ STR x
    (*:) $ STR x
evalPrintC (CELL expr NIL _)    = do
    display $ show expr
    capture expr
    (*:) expr
evalPrintC e = throwEvalError $ INVALIDForm $ show (cell (sym "printC") e)  

--------------------------------------------------
-- for debug
--------------------------------------------------

-- stacktrace
evalStackTrace :: Synt
evalStackTrace (CELL e NIL msp) = evalStackTrace $ CELL e (cell (INT (toInteger 10)) nil) msp
evalStackTrace (CELL e (CELL (INT n) NIL _) _) = do
    st <- getStackTrace
    v <- eval e
    st' <- getStackTrace
    let th = traceHeap st
        th' = traceHeap st'
    let thisst = st' >- setTraceHeap (drop (P.length th) th')
                     >- setTraceLength (fromInteger n)
    liftIO $ printStackTrace thisst
    (*:) v
evalStackTrace e = throwEvalError $ INVALIDForm $ show (cell (sym "stacktrace") e)  

-- stacktracelength
evalStackTraceLength :: Synt
evalStackTraceLength (CELL (INT n) NIL _) = do
    st <- getStackTrace
    putStackTrace $ setTraceLength (fromInteger n) st
    (*:) VOID
evalStackTraceLength e = throwEvalError $ INVALIDForm $ show (cell (sym "stackTraceLngth") e)  

-- scheme-env
evalSchemeEnv :: Proc
evalSchemeEnv (CELL (STR name) NIL _) = (*:) VOID << do
    genv <- getGEnv
    case M.lookup name genv of
      Nothing -> display $ "[env] not found: "++ name
      Just v  -> display $ "[env] "++ name ++": "++ show v
evalSchemeEnv NIL                   = (*:) VOID << do
    genv <- getGEnv
    display $ "[env Begin]"
    liftIO $ mapM_ (\(i,x) -> putStrLn $ " "++ show i ++". "++ fst x ++": "++ show (snd x)) $ P.zip [1..] (M.toList genv)
    display $ "[env End]"
evalSchemeEnv e = throwEvalError $ INVALIDForm $ show (cell (sym "scheme-env" ) e)  

--------------------------------------------------
-- show
--------------------------------------------------

-- show
evalShow :: Proc
evalShow (CELL v NIL _) = do
    liftIO $ putStr $ show v
    (*:) VOID
evalShow e = throwEvalError $ INVALIDForm $ show (cell (sym "show") e)  

-- showExpr
evalShowExpr :: Synt
evalShowExpr (CELL expr NIL _) = do
    display $ showppExpr expr
    (*:) VOID
evalShowExpr e = throwEvalError $ INVALIDForm $ show (cell (sym "showExpr") e)  

--------------------------------------------------
-- UnitTest
--------------------------------------------------

-- unittest
evalUnitTest :: Synt
evalUnitTest (CELL (STR name) (CELL pairs@(CELL _ _ _) NIL _) _) = do
    rec (L.length pairs, 0, 0, 0, "") pairs 
  where
    rec :: (Int, Int, Int, Int, String) -> Expr -> Scm ReturnE
    rec (cases, tried, 0, 0, "") NIL = do
        display $ "unittest ["++ name ++"] - "++ 
                    "Cases: "++ show cases  ++"  "++ 
                    "Tried: "++ show tried ++"  "++ 
                    "Errors: 0  "++ 
                    "Failures: 0"
        (*:) VOID
    rec (cases, tried, errors, failures, mes) NIL = do
        display $ "////////////////////////////////////////////////////////////////////"
        display $ "/// - unittest ["++ name ++"]"
        liftIO $ putStr $ mes
        display $ "/// - "++ 
                    "Cases: "++ show cases ++"  "++ 
                    "Tried: "++ show tried ++"  "++ 
                    "Errors: "++ show errors ++"  "++ 
                    "Failures: "++ show failures
        display $ "/// - unittest ["++ name ++"]" 
        display $ "////////////////////////////////////////////////////////////////////"
        (*:) $ VOID
    rec (cases, tried, errors, failures, mes) (CELL pair d _) = do
        case pair of
          CELL expr NIL msp' -> localMSP msp' $ do { 
            expected <- eval expr;
            if expected /= VOID
            then do
                let v = (\(RETURN r) -> r) expected
                let str1 = "/// ### failured in: "++ name ++": at "++ showMSP msp' ++"\n"
                    str2 = "///    tried: "++ show expr ++"\n"++ 
                           "/// expected: -"            ++"\n"++ 
                           "///  but got: "++ show v    ++"\n"
                rec (cases, tried+1, errors, failures+1, mes ++ str1 ++ str2) d     
            else do
                rec (cases, tried+1, errors, failures, mes) d } <| catch
          CELL expr (CELL answer NIL _) msp' -> localMSP msp' $ do { 
            v <- eval expr >>= catchVoid;
            if v /= answer
            then do
                let str1 = "/// ### failured in: "++ name ++": at "++ showMSP' msp' ++"\n"
                    str2 = "///    tried: "++ show expr   ++"\n"++ 
                           "/// expected: "++ show answer ++"\n"++ 
                           "///  but got: "++ show v      ++"\n"
                rec (cases, tried+1, errors, failures+1, mes ++ str1 ++ str2) d     
            else do
                rec (cases, tried+1, errors, failures, mes) d } <| catch
          CELL _ _ msp' -> localMSP msp' $ throwEvalError $ strMsg $ "invalid unittest form: pair required, but got "++ show pair 
          _             -> throwEvalError $ strMsg $ "invalid unittest form: list required, but got "++ show pairs
      where
        catch x = x `catchScmError` \e -> do
            msp' <- askMSP
            let str = "/// ### errored in: "++ name ++": at "++ showMSP' msp' ++"\n"
                      ++ (show e >- lines 
                                 >- P.foldr (\line acc -> "/// "++ line ++"\n"++ acc) "") ++"/// \n"
            rec (cases, tried+1, errors+1, failures, mes ++ str) d
            (*:) VOID
        showMSP' Nothing   = "---"
        showMSP' (Just sp) = show sp
    rec _ _ = throwEvalError $ strMsg $ "invalid unittest form: list required, but got "++ show pairs
evalUnitTest e = throwEvalError $ INVALIDForm $ show (cell (sym "unittest") e)  


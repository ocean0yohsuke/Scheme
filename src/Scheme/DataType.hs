module Scheme.DataType (
    module Scheme.DataType.Misc,

    EvalError, ScmError, TryError,
    Expr(..),
    ScmCode(..), ScmFile,

    Var(..),
    Return(..), ReturnE,
    Name, AFunc, WAFunc, RFunc, Proc, Synt, 

    Scm, runScm,
    ScmEnv, ScmStates, ScmRef,
    MetaInfo(..), Config(..), setConfig, setMSP,
    MetaData(..), setTryHeap, setStackTrace,
    GEnv, LEnv, MLEnv,
    StackTrace(..), Trace, TraceR, initStackTrace, setTraceHeap, setTraceRHeap, setTraceLength,
    Try(..), initTry, setDepthLimit, setLoopCount, setBinary, setCapturedDisplays,
    DepthLimit(..),

) where

import Config
import Scheme.DataType.Error (ScmError)
import Scheme.DataType.Error.Eval (EvalError)
import Scheme.DataType.Error.Try (TryError)
import Scheme.DataType.Misc

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.MonadTrans
import DeepControl.Monad.RWS
import DeepControl.Monad.Except
import MonadX.Monad.Reference

import qualified Data.Map as M
import Data.List (intersperse)

type Name = String 

----------------------------------------------------------------------------------------------------------------
-- Scm
----------------------------------------------------------------------------------------------------------------

type Scm a = (ExceptT ScmError
             (ReferenceT Var
             (RWST ScmEnv () ScmStates IO))) a

runScm :: Scm a 
            -> ScmRef       -- Reference
            -> ScmEnv       -- Reader
            -> ScmStates    -- State
            -> IO ((Either ScmError a, ScmRef), ScmStates, ())
runScm scm ref env states = scm >- runExceptT
                                >- (unReferenceT >-> (|>ref))
                                >- (runRWST >-> (|>env) >-> (|>states)) 

type ScmStates = (GEnv, MetaData)  -- State
type ScmEnv    = (MLEnv, MetaInfo) -- Reader 
type ScmRef    = RefEnv Var        -- Reference

--
-- Env
--
type GEnv  = M.Map Name Expr -- Global Environment
type LEnv  = Ref Var{-Vm-}   -- Local Environment
type MLEnv = Maybe LEnv

--
-- Variable for RefEnv
--
data Var = Ve Expr
         | Vm (M.Map Name Expr{-REF-})  -- for LEnv
  deriving (Eq)
instance Show Var where
    show (Ve e)     = show e
    show (Vm map)   = show map
   
--------------------------------------------------
-- Eval
--------------------------------------------------

-- TODO Functor
data Return a = RETURN a
              | VOID
  deriving (Show, Eq)

type ReturnE = Return Expr

--------------------------------------------------
-- Expr
--------------------------------------------------

-- Scheme Expression
data Expr = NIL
           | INT  !Integer 
           | REAL !Double 
           | SYM  !String MSP
           | STR  !String 
           | CELL !Expr !Expr MSP
           -- 
           | AFUNC Name AFunc   -- actual function: +, -, *, /, etc.
           | WAFUNC Name WAFunc -- weekly actual function: length, append, etc.
           | RFUNC Name RFunc   -- referencial function: car, cdr, cons, set!, set-car!, set-cdr!, etc.
           | PROC Name Proc     -- procedure: display, newline, etc.
           | SYNT Name Synt     -- syntax: quote, if, define, etc.
           | CLOS Expr MLEnv   -- closure: λ
           | CLOSM Expr MLEnv  -- macro-closure
           -- for set!, set-car!, set-cdr!, car and cdr; reference manipulation
           | REF (Ref Var{-Ve-})
           -- for lazy evaluation
           | THUNK (Expr, ScmEnv)

instance Show Expr where
    show NIL          = "()"
    show (INT x)      = show x
    show (REAL x)     = show x
    show (SYM x _)    = x
    show (STR x)      = show x
    show (CELL (SYM "quote" _) (CELL expr NIL _) _)            = "'" ++ show expr
    show (CELL (SYM "quasiquote" _) (CELL expr NIL _) _)       = "`" ++ show expr
    show (CELL (SYM "unquote" _) (CELL expr NIL _) _)          = "," ++ show expr
    show (CELL (SYM "unquote-splicing" _) (CELL expr NIL _) _) = ",@" ++ show expr
    show c@(CELL a d _) = "(" ++ showCELL c ++ ")"
      where
        showCELL NIL          = ""
        showCELL (CELL a d _) = show a ++ case d of
          NIL            -> ""
          c@(CELL _ _ _) -> " " ++ showCELL c
          e              -> " . " ++ show e
    show (AFUNC x _)   = "<" ++ x ++ ">"
    show (WAFUNC x _)  = "<" ++ x ++ ">"
    show (RFUNC x _)   = "<" ++ x ++ ">"
    show (SYNT x _)    = "<" ++ x ++ ">"
    show (PROC x _)    = "<" ++ x ++ ">"
    show (CLOS (CELL args seq _) mlenv)  = "(\\"++ show args ++" -> "++ showExprSeq seq ++")"
      where
        showExprSeq :: Expr -> String
        showExprSeq NIL            = ""
        showExprSeq (CELL s NIL _) = show s 
        showExprSeq (CELL s1 s2 _) = show s1 ++" >> "++ showExprSeq s2
        showExprSeq e              = show e
    show (CLOSM (CELL args seq _) mlenv) = "(#"++ show args ++" -> "++ showExprSeq seq ++")"
      where
        showExprSeq :: Expr -> String
        showExprSeq NIL            = ""
        showExprSeq (CELL s NIL _) = show s 
        showExprSeq (CELL s1 s2 _) = show s1 ++" >> "++ showExprSeq s2
        showExprSeq e              = show e
    show (THUNK (e, _)) = "[" ++ show e ++ "]"
    show (REF ref) = "_"

type AFunc  = Expr -> Scm Expr  -- actual function
type WAFunc = Expr -> Scm Expr  -- weekly actual function
type RFunc  = Expr -> Scm Expr  -- referencial function
type Proc   = Expr -> Scm ReturnE  -- procedure
type Synt   = Expr -> Scm ReturnE  -- syntax

instance Eq Expr where
    NIL        == NIL          = True
    INT x      == INT y        = x == y
    REAL x     == REAL y       = x == y
    SYM x _    == SYM y _      = x == y
    STR x      == STR y        = x == y
    CELL l r _ == CELL l' r' _ = (l,r) == (l',r')
    CLOS x a   == CLOS y b     = (x,a) == (y,b) 
    CLOSM x a  == CLOSM y b    = (x,a) == (y,b) 
    AFUNC x a  == AFUNC y b    = x == y
    WAFUNC x a == WAFUNC y b   = x == y
    RFUNC x a  == RFUNC y b    = x == y
    SYNT x a   == SYNT y b     = x == y 
    PROC x a   == PROC y b     = x == y 
    REF x      == REF y        = x == y
    THUNK x    == THUNK y      = x == y 
    _          == _            = False

--------------------------------------------------
-- SCode, SFile, SFiles
--------------------------------------------------

data ScmCode = EXPR Expr
             | COMMENT String
             | LINEBREAK
             | EOF

instance Show ScmCode where
  show (EXPR x)    = show x
  show (COMMENT s) = s
  show LINEBREAK   = ""

type ScmFile = [ScmCode]

----------------------------------------------------------------------------------------------------------------
-- RWS
----------------------------------------------------------------------------------------------------------------

--
-- MetaInfo 
--
data MetaInfo = MetaInfo { config :: Config
                         , msp :: MSP
                         } 
    deriving (Show, Eq)

setConfig :: Config -> MetaInfo -> MetaInfo
setConfig x (MetaInfo _ b) = MetaInfo x b
setMSP :: MSP -> MetaInfo -> MetaInfo
setMSP x (MetaInfo a _) = MetaInfo a x

--
-- MetaData 
--
data MetaData = MetaData { tryHeap :: [Try]
                         , stackTrace :: StackTrace
                         } 
    deriving (Show)

setTryHeap :: [Try] -> MetaData -> MetaData
setTryHeap x (MetaData _ b) = MetaData x b
setStackTrace :: StackTrace -> MetaData -> MetaData
setStackTrace x (MetaData a _) = MetaData a x

data StackTrace = StackTrace {
      traceHeap :: [Trace]
    , traceRHeap :: [TraceR]  -- trace rusult
    , traceLength :: Int
    }
    deriving (Show)

type Trace = (String, MSP, Maybe String)
type TraceR = Trace

initStackTrace :: StackTrace
initStackTrace = StackTrace [] [] 10

setTraceHeap :: [Trace] -> StackTrace -> StackTrace
setTraceHeap x (StackTrace _ b c) = StackTrace x b c
setTraceRHeap :: [TraceR] -> StackTrace -> StackTrace
setTraceRHeap x (StackTrace a _ c) = StackTrace a x c
setTraceLength :: Int -> StackTrace -> StackTrace
setTraceLength x (StackTrace a b _) = StackTrace a b x


-- TODO: ChaitinTry
data Try = Try { depthLimit :: DepthLimit 
               , loopCount :: Int
               , binary :: Expr
               , capturedDisplays :: [Expr]
               }
    deriving (Show)
data DepthLimit = NOTIMELIMIT 
                | DEPTHLIMIT Int
    deriving (Show, Eq)
instance Ord DepthLimit where
    compare NOTIMELIMIT    NOTIMELIMIT     = EQ
    compare NOTIMELIMIT    (DEPTHLIMIT _)  = GT
    compare (DEPTHLIMIT _) NOTIMELIMIT     = LT
    compare (DEPTHLIMIT n) (DEPTHLIMIT n') = compare n n'

initTry :: Try
initTry = Try NOTIMELIMIT 0 NIL []

setDepthLimit :: DepthLimit -> Try -> Try
setDepthLimit dl (Try _ lc bn cd) = Try dl lc bn cd
setLoopCount :: Int -> Try -> Try
setLoopCount lc (Try dl _ bn cd) = Try dl lc bn cd
setBinary :: Expr -> Try -> Try
setBinary bn (Try dl lc _ cd) = Try dl lc bn cd
setCapturedDisplays :: [Expr] -> Try -> Try
setCapturedDisplays cd (Try dl lc bn _) = Try dl lc bn cd

----------------------------------------------------------------------------------------------------------------
-- Misc 
----------------------------------------------------------------------------------------------------------------



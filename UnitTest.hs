-- {-# LANGUAGE ScopedTypeVariables #-}
module UnitTest (
	run
	) where 
import Test.HUnit

import Scheme.Evaluator
import Scheme.Action.Command (catchVoid)
import Scheme.DataType.Value
import Scheme.DataType (Expr)
import qualified Scheme.DataType
import Scheme.Parser
import qualified Scheme.LISP as L

import Text.ParserCombinators.Parsec
import qualified Data.Map as M

import MonadX.Applicative
import MonadX.Monad
import MonadX.Monad.RWS
import MonadX.Monad.Error

import System.IO.Unsafe (unsafePerformIO)


-- | Run Unit-Test of this module. 
run = runTestTT $ TestList 
        [ 
          TestList testCase_Scheme_show
        , TestList testCase_Scheme_readSchemeExpr
        , TestList testCase_Scheme_readSchemeCode

        , TestList testCase_Scheme_LISP
        , TestList testCase_Scheme_eval

    	]

-- *UnitTest> run

----------------------------------------------------------------
-- Useful staff
----------------------------------------------------------------

showScm :: Scm Expr -> String
showScm scm = 
    let ((mv,_),_,_) = unsafePerformIO $ runScm scm initScmRef initScmEnv initScmStates
    in  case mv of
        Left err -> error $ show err ++"\n"
        Right v  -> show v 

-- read & show
rs :: (Expr -> a) -> String -> a
rs func str = 
  case readSchemeExpr str of
    Left err           -> error $ show err ++"\n"
    Right (expr, [])   -> func expr
    Right (expr, rest) -> error $ "*** parser failed to exhaust whole string:\n" 
                                ++"*** parsed: "++ show expr ++"\n" 
                                ++"*** rest: " ++ show rest ++"\n"

----------------------------------------------------------------
-- unit test
----------------------------------------------------------------

--
-- DataType
-- 
testCase_Scheme_show = (\t -> "Scheme" ~: "show" ~: t) |$>
    [ (show $ INT 10)               ~?= "10"
    , (show $ REAL 1.234)           ~?= "1.234"
    , (show $ STR "hello, world")   ~?= "\"hello, world\""
    , (show $ NIL)                             ~?= "()"

    , (show $ CELL (INT 1) NIL)                ~?= "(1)"
    , (show $ CELL (INT 1) (INT 2))            ~?= "(1 . 2)"
    , (show $ CELL (INT 1) (CELL (INT 2) NIL)) ~?= "(1 2)"
    , (show $ CELL (STR "hello, world") (STR "foo bar baz"))         ~?= "(\"hello, world\" . \"foo bar baz\")"
    , (show $ CELL (INT 1) (CELL (REAL 2) NIL))                      ~?= "(1 2.0)"
    , (show $ CELL (INT 1) (CELL (REAL 2) (CELL (SYM "abc") NIL)))   ~?= "(1 2.0 abc)"
    , (show $ CELL (CELL (INT 1) NIL) NIL)                           ~?= "((1))"
    , (show $ CELL (CELL (INT 1) NIL) (CELL (CELL (INT 2) NIL) NIL)) ~?= "((1) (2))"

    , (show $ CELL NIL (INT 1))                ~?= "(() . 1)"

    , (show $ CELL (SYM "quote") (CELL (SYM "a") NIL))                              ~?= "'a"
    , (show $ CELL (SYM "quote") (SYM "a"))                                         ~?= "(quote . a)"
    , (show $ CELL (SYM "quote") (CELL (SYM "a") (SYM "b")))                        ~?= "(quote a . b)"
    , (show $ CELL (SYM "quote") (CELL (CELL (SYM "a") (CELL (SYM "b") NIL)) NIL))  ~?= "'(a b)"
    
    , (show $ CELL NIL (CELL NIL NIL))     ~?= "(() ())"
    , (show $ CELL NIL NIL)                ~?= "(())"

    , (show $ (CELL (SYM "lambda") (CELL (CELL (SYM "x") (CELL (SYM "y") NIL)) (CELL (SYM "x") NIL))))         ~?= "(lambda (x y) x)"
    , (show $ (CELL (SYM "lambda") (CELL (CELL (SYM "x") (SYM "y")) (CELL (SYM "x") NIL))))                    ~?= "(lambda (x . y) x)"
    , (show $ (CELL (SYM "lambda") (CELL (SYM "args") (CELL (SYM "x") NIL))))                                  ~?= "(lambda args x)"    
    , (show $ (CELL (SYM "lambda") (CELL (CELL (SYM "unquote") (CELL (SYM "var") NIL)) (CELL (SYM "x") NIL)))) ~?= "(lambda ,var x)"    
    ]

--
-- Parser
--
testCase_Scheme_readSchemeExpr = (\t -> "Scheme" ~: "readSchemeExpr" ~: t) |$>
    [ r "1"                ~?= "1"
    , r "1.2345"           ~?= "1.2345"
    , r "-1234"            ~?= "-1234"
    , r "abc"              ~?= "abc"
    , r "-1234"            ~?= "-1234"
    , r "-1.234"           ~?= "-1.234"
    , r "\"hello, world\"" ~?= "\"hello, world\""
    , r "(1 2 3 4)"        ~?= "(1 2 3 4)"
    , r "()"               ~?= "()"
    , r "(1 . 2)"          ~?= "(1 . 2)"
    , r "(1 2 . 3)"        ~?= "(1 2 . 3)"
    , r "((1 2) (3 4))"    ~?= "((1 2) (3 4))"
    , r "'1234"            ~?= "'1234"
    , r "'(1 2 3 4)"       ~?= "'(1 2 3 4)"

    , r "1a"            ~?= "1 +++ [a]"
    , r "()a"           ~?= "() +++ [a]"
    , r "()()"          ~?= "() +++ [()]"
    , r "(1 2) abc"     ~?= "(1 2) +++ [ abc]"

    , r "(\n1\n \n2\n)"    ~?= "(1 2)"
    , r "(\n1\n . \n2\n)"  ~?= "(1 . 2)"

    , r "(define func (lambda (x) x))" ~?= "(define func (lambda (x) x))"

    , r "`(a b c)"      ~?= "`(a b c)"
    , r "`(,a b c)"     ~?= "`(,a b c)"
    , r "`(,@a b c)"    ~?= "`(,@a b c)"

    , r "a1"                ~?= "a1"
    , r "a'"                ~?= "a'"
    , r "a?"                ~?= "a?"
    , r "a-b?"              ~?= "a-b?"
    , r "+value+"           ~?= "+value+"
    , r "++"                ~?= "++"
    , r "<+>"               ~?= "<+>"
    , r "->"                ~?= "\"scheme expression\" (line 1, column 3):\nunexpected \"->\"\nexpecting symbol"

    {-
    , r "(\\() -> 1)"        ~?= "(\\() -> 1)"
    , r "(\\(a) -> 1)"       ~?= "(\\(a) -> 1)"
    , r "(\\(a) -> 1 >> 2)"  ~?= "(\\(a) -> 1 >> 2)"
    , r "(\\(a) -> (+ a 1))" ~?= "(\\(a) -> (+ a 1))"
    -}

	]
  where
    r :: String -> String
    r input = 
      case readSchemeExpr input of
        Left err  -> show err 
        Right (expr, [])   -> show expr
        Right (expr, rest) -> show expr ++ " +++ [" ++ rest ++ "]" 


testCase_Scheme_readSchemeCode = (\t -> "Scheme" ~: "readSchemeCode" ~: t) |$>
    [ r "1\n"              ~?= "1 +++ [\n]"
    , r "1.2345\n"         ~?= "1.2345 +++ [\n]"
    , r "\n1"              ~?= " +++ [1]"
    , r "  123  \n  456  " ~?= "123 +++ [\n  456  ]"

    , r "(1 2)\n"          ~?= "(1 2) +++ [\n]"
    , r " (1 2)\n"         ~?= "(1 2) +++ [\n]"

    , r ";comment\n"       ~?= "comment"
    , r "  ;abc  \n  "     ~?= "abc  "
    , r ";front\n(1 2)\n;end\n" ~?= "front +++ [(1 2)\n;end\n]"

    ]
  where
    r :: String -> String
    r input = 
        case readSchemeCode input of
            Left err  -> show err 
            Right (expr, [])   -> show expr
            Right (expr, rest) -> show expr ++ " +++ [" ++ rest ++ "]" 

testCase_Scheme_LISP = (\t -> "Scheme" ~: "LISP" ~: t) |$>
    [
      (show $ L.append NIL 
                       (CELL (INT 1) NIL))                ~?= "(1)"
    , (show $ L.append NIL NIL)                           ~?= "()"
    , (show $ L.append (CELL (INT 1) NIL) 
                       NIL)                               ~?= "(1)"
    , (show $ L.append (CELL (INT 1) NIL) 
                       (CELL (INT 2) NIL))                ~?= "(1 2)"
    , (show $ L.append (CELL (INT 1) (CELL (INT 2) (CELL (INT 3) NIL))) 
                       (CELL (INT 4) (CELL (INT 5) NIL))) ~?= "(1 2 3 4 5)"

    , (show $ L.toList (CELL (INT 1) (CELL (INT 2) (CELL (INT 3) NIL)))) ~?= "[1,2,3]"
    , (show $ L.fromList [INT 1, INT 2, INT 3]) ~?= "(1 2 3)"
    , (show $ L.mapM (\x -> Just x) (CELL (INT 1) NIL)) ~?= "Just (1)"
    --, (show $ L.mapM (\x -> Just x) (CELL (INT 1) (INT 2))) ~?= "Just (1 . 2)"

    , L.isList (NIL)                  ~?= True
    , L.isList (CELL (INT 1) NIL)     ~?= True
    , L.isList (CELL (INT 1) (INT 1)) ~?= False

    , L.length (CELL (INT 1) NIL)     ~?= 1

    , (show $ L.mapM (\x -> Just x) (CELL (INT 1) (CELL (INT 2) NIL))) ~?= "Just (1 2)"
   ]

testCase_Scheme_eval = (\t -> "Scheme" ~: "eval" ~: t) |$>
    [ t "3"               ~?= "3", 
      t "(+ 1 2)"         ~?= "3",
      t "(+ (+ 1 2) 3 4)" ~?= "10"
    ]
  where
    t = rs $ (eval >=> catchVoid) >-> showScm




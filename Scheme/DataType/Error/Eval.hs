module Scheme.DataType.Error.Eval where

import MonadX.Monad.Error

data EvalError = VOIDErr
               | MULTDecl String     -- multiple declaration
               | UNBOUNDVar String   -- unbound variable
               | DIVIDEDByZero String
               | WRONGNumArgs String
               | WRONGTypeArg String
               | INVALIDForm String
               | INVALIDValue String
               | ILLArgListReq String
               | ILLArgIntReq String
               | ILLArgNumReq String
               | BINDErr String
               | OTHER String

instance Error EvalError where
    strMsg s = OTHER s

instance Show EvalError where
    show VOIDErr           = "Void returned."
    show (MULTDecl s)      = "multiple declarations of '" ++ s ++ "'"
    show (UNBOUNDVar s)    = "unbound variable: " ++ s
    show (DIVIDEDByZero s) = "divided by zero: " ++ s
    show (WRONGNumArgs s)  = "wrong number of arguments: " ++ s
    show (WRONGTypeArg s)  = "wrong type of argument: " ++ s
    show (INVALIDForm s)   = "invalid form: " ++ s
    show (INVALIDValue s)  = "invalid value: " ++ s
    show (ILLArgListReq s) = "Illegal argument, List required, but got: " ++ s
    show (ILLArgIntReq s)  = "Illegal argument, Integer required, but got: " ++ s
    show (ILLArgNumReq s)  = "Illegal argument, Number required, but got: " ++ s
    show (BINDErr s)       = "bind error: " ++ s
    show (OTHER s)         = s




module Scheme.DataType.Error where

import Scheme.DataType.Error.Eval (EvalError)
import Scheme.DataType.Error.Try (TryError)
import Scheme.DataType.Misc

import MonadX.Monad.Error

--------------------------------------------------
-- Data
--------------------------------------------------

data ScmError = EXITErr
              | EVALErr MSP EvalError
              | TRYErr TryError
              | OTHER String

instance Error ScmError where
    strMsg s = OTHER s

--------------------------------------------------
-- Show
--------------------------------------------------

instance Show ScmError where
    show EXITErr                 = "Exited."
    show (EVALErr (Just sp) err) = "Eval error: " ++ (showSourcePos sp) ++ "\n" ++ show err
    show (EVALErr Nothing err)   = "Eval error: " ++ show err
    show (TRYErr err)            = show err
    show (OTHER s)               = "Scheme error: " ++ s 


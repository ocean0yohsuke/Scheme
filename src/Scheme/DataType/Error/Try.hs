module Scheme.DataType.Error.Try where

import DeepControl.Monad.Except

-- for Chaitin's Omega function
data TryError = OUTOFDATA
              | OUTOFTIME
              | PARSEErr String
              | OTHER String 

instance Error TryError where
    strMsg s = OTHER s

instance Show TryError where
    show OUTOFDATA    = "out-of-data"
    show OUTOFTIME    = "out-of-time"
    show (PARSEErr s) = "failed to parse: " ++ s
    show (OTHER s)    = show s


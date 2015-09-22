{-# LANGUAGE BangPatterns #-}
module Scheme.Evaluator.Chaitin where

import Scheme.DataType.Error.Eval
import Scheme.DataType.Error.Try
import Scheme.Evaluator.Micro
import Scheme.Parser
import Util.Bits

import qualified Prelude as P (foldr, filter, length)
import Prelude hiding (foldr, filter, length)
import Scheme.LISP as L

import Data.Char
import Data.List
import qualified Data.Map as M

-- for debug
import Debug.Trace


toBit (INT 0) = Zero
toBit (INT 1) = One

--------------------------------------------------
-- GEnv
--------------------------------------------------

chaitinGEnv :: GEnv
chaitinGEnv = M.fromList [ 
      ("size", AFUNC "size" evalSize)
    , ("++", AFUNC "++" evalAppendStr)
    , ("bits", AFUNC "bits" evalBits)
    , ("base10-to-2", AFUNC "base10-to-2" evalBase10to2)
    , ("base2-to-10", AFUNC "base2-to-10" evalBase2to10)
    , ("try", AFUNC "try" evalTry)
    , ("capture", AFUNC "capture" evalCapture)
    , ("read-bit", AFUNC "read-bit" evalReadBit)
    , ("read-exp", AFUNC "read-exp" evalReadExp)

   ]

--------------------------------------------------
-- 
--------------------------------------------------

-- size
evalSize :: AFunc
evalSize (CELL v NIL _) = (*:) $ INT $ v >- (show >-> P.length >-> toInteger)
evalSize e = throwEvalError $ INVALIDForm $ show (cell (sym "size") e) 

-- ++
evalAppendStr :: AFunc
evalAppendStr xs = foldlM append (STR "") xs
  where
    append :: Expr -> AFunc
    append (STR x) (STR y) = (*:) $ STR (x ++ y)
    append x       y       = throwEvalError $ WRONGTypeArg $ show (cell (sym "++") xs)  

-- bits 
evalBits :: AFunc
evalBits c@(CELL s NIL _) = do
    bytes <- unJustList $ toByte . ord |$> show s
    let newline = "00001010"
        byteString = P.foldr (++) newline $ bytes <$| show
    (*:) $ fromList $ byteString <$| (INT <-< toInteger <-< digitToInt)
  where
    unJustList :: [Maybe Bits] -> Scm [Bits]
    unJustList [] = (*:) []
    unJustList ((Just x):xs) = (x:) |$> unJustList xs
    unJustList (Nothing:_)   = throwEvalError $ strMsg $ "bits: failed to convert '"++ show c ++"'"
evalBits e = throwEvalError $ INVALIDForm $ show (cell (sym "bits") e)  

-- base10-to-2
evalBase10to2 :: AFunc
evalBase10to2 c@(CELL (INT n) NIL _) = do
    byte <- n >- (fromInteger >-> toBinary >-> unJust)
    let byteString = show byte
    (*:) $ fromList $ byteString <$| (INT <-< toInteger <-< digitToInt)
  where
    unJust :: Maybe Bits -> Scm Bits
    unJust (Just x) = (*:) x
    unJust Nothing  = throwEvalError $ strMsg $ "base10-to-2: failed to convert '"++ show c ++"'"
evalBase10to2 e = throwEvalError $ INVALIDForm $ show (cell (sym "base10-to-2") e)  

-- base2-to-10
evalBase2to10 :: AFunc
evalBase2to10 c@(CELL ns NIL _) = do
    bits <- toBits ns
    INT |$> (bits >- (bitsToInteger >-> unJust))
  where
    toBits :: Expr -> Scm Bits
    toBits (CELL (INT 0) NIL _) = (*:) $ Bits [Zero]
    toBits (CELL (INT 1) NIL _) = (*:) $ Bits [One]
    toBits (CELL (INT 0) xs _)  = toBits xs <$| (\(Bits xs) -> Bits $ Zero:xs)
    toBits (CELL (INT 1) xs _)  = toBits xs <$| (\(Bits xs) -> Bits $ One:xs)
    toBits e = throwEvalError $ INVALIDForm $ show (cell (sym "base2-to-10") c)  
    unJust :: Maybe Integer -> Scm Integer
    unJust (Just x) = (*:) x
    unJust Nothing  = throwEvalError $ strMsg $ "base2-to-10: failed to convert "++ show c 
evalBase2to10 e = throwEvalError $ INVALIDForm $ show (cell (sym "base2-to-10") e)  

--------------------------------------------------
-- try
--------------------------------------------------

-- (try depth-limit lisp-expression binary-data)
--   -> (success/failure value/out-of-time/out-of-data captured-displays)
-- depth limit: (limit on the nesting depth of function calls and re-evaluations).
evalTry :: AFunc
evalTry c@(CELL times (CELL expr (CELL binary NIL _) _) _) = do
    dl <- toDepthLimit times
    pushTry $ Try dl 0 binary []
    result <- runEval expr
    case result of
      ((v, ref'), states', _) -> do
        --traceM $ "try: states': "
        --traceM $ "try: v: "++ show v
        putScmStates states'
        putRefEnv ref'
        mtry <- popTry
        case mtry of
          Nothing  -> throwTryError OUTOFDATA
          Just try -> do
            let cd = fromList $ capturedDisplays try
            case v of
              Left err -> case err of
                  -- TryError
                  TRYErr err -> case err of
                      OUTOFDATA -> (*:) $ cell (sym "failure") (cell (sym "out-of-data") (cell cd nil))
                      OUTOFTIME -> do
                        mtry <- getTry
                        case mtry of
                          Nothing      -> (*:) $ cell (sym "failure") (cell (sym "out-of-time") (cell cd nil))
                          Just pre_try -> do
                            if depthLimit pre_try < depthLimit try
                            then throwTryError OUTOFTIME
                            else (*:) $ cell (sym "failure") (cell (sym "out-of-time") (cell cd nil))
                      PARSEErr _ -> (*:) $ cell (sym "success") (cell (sym "parse-error") (cell cd nil))
                  -- EvalError
                  EVALErr _ err -> (*:) $ cell (sym "success") (cell (cell (sym (show err)) nil) (cell cd nil))
                  err           -> (*:) $ cell (sym "success") (cell (cell (sym (show err)) nil) (cell cd nil))
              Right v -> case v of
                  VOID     -> (*:) $ cell (sym "success") (cell (sym "<void>") (cell cd nil))
                  RETURN v -> (*:) $ cell (sym "success") (cell v (cell cd nil))
  where
    toDepthLimit (INT n)                 = (*:) $ DEPTHLIMIT (fromInteger n)
    toDepthLimit (SYM "no-time-limit" _) = (*:) NOTIMELIMIT
    toDepthLimit e                       = throwEvalError $ INVALIDForm $ show (cell (sym "try") e)
    runEval expr = do
        env <- askScmEnv
        states <- getScmStates
        ref <- getRefEnv
        liftIO $ runScm (expr >- ({-evalMacro >=>-} eval)) ref env states
evalTry e = throwEvalError $ INVALIDForm $ show (cell (sym "try") e)  

-- capture
evalCapture :: AFunc
evalCapture (CELL (STR x) NIL _) = do
    capture $ STR x
    (*:) $ STR x
evalCapture (CELL expr NIL _)    = do
    capture expr
    (*:) expr
evalCapture e = throwEvalError $ INVALIDForm $ show (cell (sym "capture") e)  


{-
-- capture
evalCapture :: Proc
evalCapture (CELL (STR x) NIL _) = do
    capture $ STR x
    (*:) $ RETURN $ STR x
evalCapture (CELL expr NIL _)    = do
    capture expr
    (*:) $ RETURN $ expr
evalCapture e = throwEvalError $ INVALIDForm $ show (cell (sym "capture") e)  
-}

--------------------------------------------------
-- read
--------------------------------------------------

-- read-bit
evalReadBit :: AFunc
evalReadBit NIL = do
    mtry <- popTry
    case mtry of
      Nothing  -> throwTryError OUTOFDATA
      Just try -> do
        let bits = binary try
        case bits of
          NIL        -> do
            pushTry try
            throwTryError OUTOFDATA
          CELL _ _ _ -> do
            (bit, rest) <- readBit bits try 
            pushTry $ setBinary rest try
            (*:) bit
          e        -> do
            pushTry try
            throwEvalError $ INVALIDValue $ show (cell (sym "read-bit") e)  
  where
    readBit (CELL (INT 0) rest _) _   = (*:) (INT 0, rest)
    readBit (CELL (INT 1) rest _) _   = (*:) (INT 1, rest)
    readBit e                     try = do
        pushTry try
        throwEvalError $ INVALIDValue $ show (cell (sym "read-bit") e)  
evalReadBit e = throwEvalError $ INVALIDForm $ show (cell (sym "read-bit") e)  

-- read-exp
evalReadExp :: AFunc
evalReadExp NIL = do
    mtry <- popTry
    case mtry of
      Nothing  -> throwTryError OUTOFDATA
      Just try -> do
        let bits = binary try
        case bits of
          NIL      -> do
            pushTry try
            throwTryError OUTOFDATA
          CELL _ _ _ -> do
            let bitList = toList bits
            mv <- splitAtNewline bitList
            case mv of
              Nothing            -> do
                pushTry try
                throwTryError OUTOFDATA
              Just (bytes, rest) -> do
                pushTry $ setBinary (fromList rest) try
                let string = P.filter (isAscii <$|(&&)|*> isPrint) $ bytes <$| (bitsToInteger >-> (\(Just x) -> x) >-> fromInteger >-> chr)
                if string == []
                then (*:) NIL
                else case readSchemeExpr string of
                  Left err           -> throwTryError $ PARSEErr string
                  Right (expr, [])   -> (*:) expr
                  Right (expr, rest) -> throwTryError $ PARSEErr string
          e        -> do
            pushTry try
            throwEvalError $ INVALIDValue $ show (cell (sym "read-exp") e)  
  where
    splitAtNewline :: [Expr] -> Scm (Maybe ([Byte], [Expr]))
    splitAtNewline ls = iter ls []
      where
        iter :: [Expr] -> [Byte] -> Scm (Maybe ([Byte], [Expr]))
        iter ls bytes = do
            let newline = [INT 0, INT 0, INT 0, INT 0, INT 1, INT 0, INT 1, INT 0]
                (byte, rest) = splitAt 8 ls
            if P.length byte < 8 
            then (*:) Nothing 
            else do
            if byte == newline
            then (*:) $ Just (bytes, rest)
            else iter rest (bytes ++ [Bits (toBit |$> byte)])



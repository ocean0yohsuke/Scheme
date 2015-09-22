module Scheme.Action.IO where

import Scheme.DataType.Misc
import Scheme.DataType
import Scheme.Util
import Scheme.Action.Command
import Scheme.Action.Env
import Scheme.Parser

import MonadX.Applicative
import MonadX.Monad
import MonadX.MonadTrans
import MonadX.Monad.Error

import Data.List (intersperse)
import System.IO 

-- for debug
import Debug.Trace 

----------------------------------------------------------------------------------------------------------
-- load
----------------------------------------------------------------------------------------------------------

loadSchemeCodes :: Filename -> IO (Either ParseError [Expr])
loadSchemeCodes path = 
    withFile path ReadMode $ \h -> do  -- TODO: エラー処理
        contents <- hGetContents h
        case readSchemeFile path contents of
            Left err         -> (*:) $ Left err 
            Right (codes, _) -> (**:) $ codes >- filter isEXPR 
                                              >>= (\(EXPR x) -> [x])
  where
    isEXPR (EXPR _) = True  
    isEXPR _        = False

----------------------------------------------------------------------------------------------------------
-- printStackTrace
----------------------------------------------------------------------------------------------------------

printStackTrace :: StackTrace -> IO ()
printStackTrace st = do
    let th = traceHeap st
        trh = traceRHeap st
        len = traceLength st
    if len == 0
    then (*:) ()
    else do
    let (th_offset, trh_offset) = offsets len th trh
        th' = case th of
            [] -> ""
            xs -> reverse (zip [((length trh)+1)..] (reverse xs))
                  >- takeStack th_offset
                  >- fmap (\(i, (eval, msp, _)) -> " -" ++ show i ++ ". " ++ showMSourcePos msp ++ "\n"++ eval)
                  >- (\xs -> if abs th_offset < length th
                             then if th_offset > 0 
                                  then xs ++ [" ..."]
                                  else " ..." : xs
                             else xs)
                  >- (intersperse "\n\n" >-> foldl (++) "")
        trh' = case trh of
            [] -> ""
            xs -> reverse (zip [1..] (reverse xs))
                  >- takeStack trh_offset
                  >- fmap (\(i, (eval, msp, Just v)) -> " -" ++ show i ++ ". " ++ showMSourcePos msp ++ "\n"++ eval ++ "\n => " ++ v)
                  >- (\xs -> if abs trh_offset < length trh
                             then if trh_offset > 0 
                                  then xs ++ [" ..."]
                                  else " ..." : xs
                             else xs)
                  >- (intersperse "\n\n" >-> foldl (++) "")
    let th''  = putStrLn ("_______________ Stack Trace [" ++ show (length th) ++ "] _______________\n" ++ th')
        trh'' = putStrLn ("__________ Stack Trace Resulted [" ++ show (length trh) ++ "] __________\n" ++ trh')
        end   = putStrLn ("______________________________________________\n")
    print len (th'', trh'', end) (abs th_offset, abs trh_offset)
  where
    offsets len th trh = 
        if len > 0
        then let th_offset  = min len (length th)
                 trh_offset = min (length trh) (len - th_offset)
             in (th_offset, trh_offset)
        else let trh_offset = max len ((-1)*(length trh))
                 th_offset  = min (length th) (len - trh_offset)
             in (th_offset, trh_offset)
    print len (th, trh, end) (th_offset_len, trh_offset_len)
        | len == 0 = (*:) ()
        | len > 0  = do
            if th_offset_len == 0
            then trh >> end
            else if trh_offset_len == 0
                 then th >> end
                 else th >> trh >> end
        | len < 0  = do
            if trh_offset_len == 0
            then th >> end
            else if th_offset_len == 0
                 then trh >> end
                 else trh >> th >> end
    takeStack :: Int -> [a] -> [a] 
    takeStack len xs
      | len == 0 = []
      | len > 0  = take len xs
      | len < 0  = reverse $ take (abs len) (reverse xs)
    showMSourcePos (Just sp) = "[" ++ showSourcePos sp ++ "]"
    showMSourcePos Nothing   = "[ --- ]"

----------------------------------------------------------------------------------------------------------
-- for debug
----------------------------------------------------------------------------------------------------------

display :: String -> Scm ()
display = liftIO . putStrLn

displayScm :: String -> Expr -> Scm ()
displayScm [] e = do
    refenv <- getScmRef
    display $ show e
displayScm s e = do
    refenv <- getScmRef
    display $ s ++ ": " ++ show e

printGEnv :: Name -> Scm ()
printGEnv name = do
    mv <- lookupGEnv name
    case mv of
      Nothing -> throwScmError $ strMsg $ "[printGEnv] not found: " ++ name
      Just v  -> display $ "[printGEnv] " ++ name ++ ": " ++ show v
 

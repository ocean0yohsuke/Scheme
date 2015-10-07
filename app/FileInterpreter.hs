import Config
import Scheme.DataType
import Scheme.Util
import Scheme.Parser
import Scheme.Evaluator
import Scheme.Action (loadSchemeCodes, printStackTrace, refreshStackTrace)

import DeepControl.Applicative
import DeepControl.Monad
import DeepControl.Arrow
import MonadX.MonadTrans
import MonadX.Monad.RWS
import Util.FileSystem

import System.IO
import Data.List 
import Data.Char (isDigit)
import Data.Function (on)
import System.Directory (readable)
import System.FilePath ((</>), takeExtension, takeDirectory)

main :: IO ()
main = run

run :: IO ()
run = do
    let sd = startDir initConfig
    fdInfos <- scmFDInfos sd
    (runRWST runFI) fdInfos (sd, Nothing)
    (*:) ()

scmFDInfos :: FilePath -> IO [FDInfo]
scmFDInfos fp = do
    x <- toFDInfo fp
    xs <- lookoverP ((isScmFile <$|(||)|*> isDirectory) <$|(&&)|*> isReadable) (sortBy compare) fp
    (*:) (x:xs)
  where
    isScmFile :: FDInfo -> Bool
    isScmFile = extension <$|(==".scm")
      where
        extension :: FDInfo -> String 
        extension = takeExtension |$> fdPath
    isReadable :: FDInfo -> Bool
    isReadable = readability <$|(==(Just True))
      where
        readability :: FDInfo -> Maybe Bool
        readability = readable |$>> fdPerms

--------------------------------------------------------------------------
-- FileInterpreter
--------------------------------------------------------------------------

type CD = String         -- Current Dirctory
type FN = Maybe Int      -- File Number
type Vars = (CD, FN)
type FileInterpreter a = RWST [FDInfo] () Vars IO a

putCD :: CD -> FileInterpreter ()
putCD x = do
    (cd, n) <- get
    put (x, n)
getCD :: FileInterpreter CD
getCD = do
    (cd, n) <- get
    (*:) cd
putFN :: FN -> FileInterpreter ()
putFN x = do
    (cd, n) <- get
    put (cd, x)
getFN :: FileInterpreter FN
getFN = do
    (cd, n) <- get
    (*:) n

runFI :: FileInterpreter ()
runFI = do 
    cd <- getCD
    fdInfos <- liftIO $ scmFDInfos cd
    local (const fdInfos) $ showFilenames >> loop
  where 
    showFilenames :: FileInterpreter ()
    showFilenames = do 
        fdInfos <- ask
        cd <- getCD
        let paths = fdInfos <$| (fdPath &&& isDirectory >>> (neat cd))
        liftIO $ putStrLn $ "Scheme Files in '" ++ cd ++ "'."
        liftIO $ putStrLn "Input number or command; 'list', 'up' or 're-eval' (abbr. ':l', ':u', ':r', respectively)."
        liftIO $ mapM_ (\(i,path) -> putStrLn $ "- "++ show i ++". "++ path) $ zip [0..] paths
      where
        neat cd (path, isdir)
            | path == (cd</>"") = "../"
            | isdir             = "[" ++ drop (length cd + 1) path ++ "]"
            | otherwise         = drop (length cd + 1) path
    loop :: FileInterpreter ()
    loop = do
        fdInfos <- ask
        cd <- getCD
        liftIO $ putStr $ "[" ++ cd ++ "]: "
        input <- liftIO getLine
        if null input then loop 
        else if input `elem` ["l", ":l", "list"] then runFI 
             else if input `elem` ["u", ":u", "up"] then upDirectory
                  else if input `elem` ["r", ":r", "re-eval"]
        then do fn <- getFN
                case fn of
                  Just n  -> evalFile (fdInfos!!n)
                  Nothing -> runFI
        else if not (and $ isDigit |$> input) then loop 
             else let n = (read input :: Int)
                  in if n >= length fdInfos then loop 
                     else let fdInfo = fdInfos!!n
                          in if fdPath fdInfo == cd then upDirectory 
                             else if isDirectory fdInfo then diveDirectory fdInfo 
                                  else evalFile fdInfo
      where
        upDirectory :: FileInterpreter ()
        upDirectory = do
            putFN Nothing
            cd <- getCD
            putCD $ takeDirectory cd
            runFI
        diveDirectory :: FDInfo -> FileInterpreter ()
        diveDirectory fdInfo = do
            putFN Nothing
            putCD $ fdPath fdInfo 
            runFI
        evalFile :: FDInfo -> FileInterpreter ()
        evalFile fdInfo = do
            fdInfos <- ask
            putFN $ fdInfo `elemIndex` fdInfos
            runREPL fdInfo
            loop 

----------------------------------------------------------------------------------------------------------------
-- REP (read - eval - print - loop)
----------------------------------------------------------------------------------------------------------------

runREPL :: FDInfo -> FileInterpreter ()
runREPL fdInfo = do
    fn <- getFN
    case fn of
      Just n -> do
        let path = fdPath fdInfo
        mmv <- liftIO $ loadSchemeCodes path
        liftIO $ case mmv of   
          Left err -> do
            putStrLn "" *> putStrLn ("*** Parse error: ")
            putStr $ show err
            putStrLn "" *> putStrLn ("________________________________________")
          Right codes -> do
            putStrLn $ "[ begin: " ++ show n ++ ". " ++ path ++ " ]"
            iter (initScmStates, initScmRef) codes
            putStrLn $ "[ end: " ++ show n ++ ". " ++ path ++ " ]"
      Nothing -> runFI
    where
      iter :: (ScmStates, ScmRef) -> [Expr] -> IO ()
      iter _             []     = (*:) ()
      iter (states, ref) (c:cs) = do
          v <- c >- (evalMacro >=> eval)
                 >- \x -> runScm x ref initScmEnv (refreshStackTrace states)
          case v of
            ((Left err, ref'), states', _) -> error err states' ref'
            ((Right v,  ref'), states', _) -> case v of
                VOID     -> iter (states', ref') cs
                RETURN v -> do
                    putStr "=> "
                    putStrLn $ show v
                    iter (states', ref') cs
        where
          error err states ref = do
              putStr "*** "
              putStrLn $ show err
              let (_, metadata) = states
              printStackTrace (stackTrace metadata)
              iter (states, ref) cs 




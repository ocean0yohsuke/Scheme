import Scheme.DataType
import Scheme.Util
import Scheme.Parser
import Scheme.Evaluator
import Scheme.Action (printStackTrace, refreshStackTrace)

import DeepControl.Applicative
import DeepControl.Monad

import System.IO

main :: IO ()
main = run

run :: IO ()
run = do
    xs <- hGetContents stdin
    putStr "Scheme> "
    putStrLn ""
    repl (initScmEnv, initScmStates, initScmRef) xs

----------------------------------------------------------------------------------------------------------------
-- REPL (read - eval - print - loop)
----------------------------------------------------------------------------------------------------------------

-- read-eval-print-loop
repl :: (ScmEnv, ScmStates, ScmRef) -> String -> IO ()
repl (env, states, ref) xs = do
    hFlush stdout
    case readSchemeCode xs of
      Left e                  -> do
        putStrLn "" *> putStrLn ("*** Read error: ")
        putStr $ show e
        putStrLn "" *> putStrLn ("________________________________________")
        repl (env, states, ref) (tail xs)
      Right (COMMENT _, rest) -> repl (env, states, ref) rest
      Right (LINEBREAK, rest) -> repl (env, states, ref) rest
      Right (EOF, _)          -> (*:) ()
      Right (EXPR expr, rest) -> do 
        v <- runScm (evalMacro >=> eval $ expr) ref env states
        case v of
          ((Left err, ref'), states', _) -> error err states' ref' rest
          ((Right v, ref'), states', _)  -> case v of
            VOID     -> repl (env, refreshStackTrace states', ref') rest
            RETURN v -> do 
              putStrLn $ "=> " ++ show v
              repl (env, refreshStackTrace states', ref') rest
  where
    error err states ref rest = do    
        putStrLn $ "*** " ++ show err
        let (_, metadata) = states
        printStackTrace (stackTrace metadata)
        repl (env, refreshStackTrace states, ref) rest


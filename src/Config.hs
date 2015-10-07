module Config where

data Config = Config {
          startDir :: String
        , isLazyMode :: Bool
        , strictlyEvalFunc :: [String]
        }
    deriving (Show, Eq)

initConfig = Config {
      isLazyMode = True
    , startDir = "Start"    
    , strictlyEvalFunc = ["capture"]
    }



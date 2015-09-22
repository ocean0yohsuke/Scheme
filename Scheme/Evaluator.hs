module Scheme.Evaluator (
    eval, evalMacro,
    runScm, Scm,
    initScmStates,
    initScmEnv,
    initScmRef,
    ) where

import qualified Config as Cfg
import MonadX.Monad.Reference (initReference)

import Scheme.DataType
import Scheme.Evaluator.Micro
import Scheme.Evaluator.Prelude
import Scheme.Evaluator.Prelude2
import Scheme.Evaluator.IO
import Scheme.Evaluator.Chaitin

import Text.Parsec (SourceName)
import qualified Data.Map as M

initGEnv :: GEnv
initGEnv =    microGEnv 
    `M.union` preludeGEnv 
    `M.union` prelude2GEnv 
    `M.union` chaitinGEnv
    `M.union` ioGEnv 

initScmStates :: ScmStates
initScmStates = (initGEnv, initMetaData)
  where
    initMetaData :: MetaData
    initMetaData = MetaData [] initStackTrace

initScmEnv :: ScmEnv
initScmEnv = (Nothing, initMetaInfo)
  where
    initMetaInfo :: MetaInfo
    initMetaInfo = MetaInfo initConfig Nothing

initScmRef :: ScmRef
initScmRef = initReference "Scm"


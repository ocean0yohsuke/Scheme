module Scheme.Action.Env where

import Config
import Scheme.DataType
import Scheme.Action.Command
import Scheme.Util

import DeepControl.Applicative
import DeepControl.Monad

import qualified Data.Map as M

--------------------------------------------------------------------------
-- Environment
--------------------------------------------------------------------------

--
-- Local Environment
--
pushLEnv :: Name -> Expr -> LEnv -> Scm ()
pushLEnv name v lenv = do
    Vm map <- readVarRef lenv
    ref <- newVarRef (Ve v)
    writeVarRef lenv $ Vm (M.insert name (REF ref) map)

lookupLEnv :: Name -> LEnv -> Scm (Maybe Expr{-REF-})
lookupLEnv name lenv = do
    Vm map <- readVarRef lenv
    (*:) $ M.lookup name map

newLEnv :: MLEnv -> Scm LEnv
newLEnv mlenv = do
    case mlenv of
      Nothing   -> newVarRef (Vm M.empty)
      Just lenv -> lenv >- (readVarRef >=> newVarRef)

--
-- Global Environment
--
pushGEnv :: Name -> Expr -> Scm ()
pushGEnv name v = do
    ref <- newVarRef (Ve v)
    genv <- getGEnv
    putGEnv $ M.insert name (REF ref) genv

lookupGEnv :: Name -> Scm (Maybe Expr{-REF-})
lookupGEnv name = do
    genv <- getGEnv
    case M.lookup name genv of
      Nothing        -> (*:) Nothing
      Just r@(REF _) -> (*:) $ Just r
      Just v         -> do
        pushGEnv name v
        lookupGEnv name

{-
pushGEnv :: Name -> Expr -> Scm ()
pushGEnv name v = do
    genv <- getGEnv
    putGEnv $ M.insert name v genv

lookupGEnv :: Name -> Scm (Maybe Expr)
lookupGEnv name = do
    genv <- getGEnv
    (*:) $ M.lookup name genv
-}


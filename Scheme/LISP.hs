module Scheme.LISP (
  LISP(..), seqM,
) where

import MonadX.Applicative
import MonadX.Monad

import Scheme.DataType.Value (Value)
import qualified Scheme.DataType.Value as V
import Scheme.DataType
import Util.LISP

instance LISP Value where
    car (V.CELL a _) = a
    car _            = error "car: empty structure"
    cdr (V.CELL _ d) = d
    cdr _            = error "cdr: empty structure"
    cons = V.CELL
    nil  = V.NIL
    isCell (V.CELL _ _) = True
    isCell _            = False

instance LISP Expr where
    car (CELL a _ _) = a
    car _            = error "car: empty structure"
    cdr (CELL _ d _) = d
    cdr _            = error "cdr: empty structure"
    cons a d = CELL a d Nothing
    nil = NIL
    isCell (CELL _ _ _) = True
    isCell _            = False

seqM :: (Applicative m, Monad m, LISP l) => (l -> m (Return a)) -> l -> m (Return a)
seqM proc = foldlM (\_ x -> proc x) VOID


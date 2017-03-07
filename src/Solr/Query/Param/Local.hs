module Solr.Query.Param.Local where

import Solr.Expr.Type (ExprTy)

import GHC.Exts (Constraint)

data family LocalParam
  (sym :: (ExprTy -> *) -> ((ExprTy -> *) -> *) -> Constraint)

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

-- | An initial encoding of a Solr query. This is an alternative interpretation
-- of the Solr language that is more amenable to parsing from arbitrary user
-- input.

module Solr.Query.Initial.Internal where

import Solr.Expr.Initial.Typed   (typeCheckSolrExpr)
import Solr.Internal.Class.Query
import Solr.Query.Param

import qualified Solr.Expr.Initial.Untyped as Untyped
import qualified Solr.Expr.Initial.Typed   as Typed

import Data.Text (Text)


-- | A Solr query.
data SolrQuery expr
  = forall a. QDefaultField (expr a)
  | forall a. QField Text (expr a)
  | QAnd (SolrQuery expr) (SolrQuery expr)
  | QOr (SolrQuery expr) (SolrQuery expr)
  | QNot (SolrQuery expr) (SolrQuery expr)
  | QScore (SolrQuery expr) Float
  | QNeg (SolrQuery expr)
  | QParams [Param SolrQuery] (SolrQuery expr)

instance SolrExprSYM expr => SolrQuerySYM expr SolrQuery where
  data ParamKey SolrQuery a where
    SolrQueryDefaultField :: ParamKey SolrQuery Text
    SolrQueryOp           :: ParamKey SolrQuery Text

  defaultField = QDefaultField
  (=:)         = QField
  (&&:)        = QAnd
  (||:)        = QOr
  (-:)         = QNot
  (^=:)        = QScore
  neg          = QNeg
  params       = QParams

instance HasParamDefaultField SolrQuery where
  paramDefaultField = SolrQueryDefaultField

instance HasParamOp SolrQuery where
  paramOp = SolrQueryOp


-- | Type check an untyped Solr query. Note the 'Untyped.SolrExpr' on the way in
-- is not the same as the 'Typed.SolrExpr' on the way out.
typeCheckSolrQuery :: SolrQuery Untyped.SolrExpr -> Maybe (SolrQuery Typed.SolrExpr)
typeCheckSolrQuery u0 =
  case u0 of
    QDefaultField u -> typeCheckSolrExpr u (fmap QDefaultField)

    QField s u -> typeCheckSolrExpr u (fmap (QField s))

    QAnd u1 u2 -> binop QAnd u1 u2
    QOr  u1 u2 -> binop QOr  u1 u2
    QNot u1 u2 -> binop QNot u1 u2

    QScore u n -> do
      q <- typeCheckSolrQuery u
      pure (QScore q n)

    QNeg u -> do
      q <- typeCheckSolrQuery u
      pure (QNeg q)

    QParams ps u -> do
      q <- typeCheckSolrQuery u
      pure (QParams ps q)
 where
  binop
    :: (SolrQuery Typed.SolrExpr -> SolrQuery Typed.SolrExpr -> SolrQuery Typed.SolrExpr)
    -> SolrQuery Untyped.SolrExpr
    -> SolrQuery Untyped.SolrExpr
    -> Maybe (SolrQuery Typed.SolrExpr)
  binop con u1 u2 = do
    q1 <- typeCheckSolrQuery u1
    q2 <- typeCheckSolrQuery u2
    pure (con q1 q2)

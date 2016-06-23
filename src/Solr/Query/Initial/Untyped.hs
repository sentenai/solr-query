{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}

module Solr.Query.Initial.Untyped where

import Solr.Class
import Solr.Param
import Solr.Type

import Data.Text (Text)


-- | An untyped Solr expression.
data SolrExpr (a :: SolrType)
  = ENum Float
  | ETrue
  | EFalse
  | EWord Text
  | EWild Text
  | ERegex Text
  | forall b. EPhrase [SolrExpr b]
  | forall b. EFuzz (SolrExpr b) Int
  | forall b. ETo (Boundary (SolrExpr b)) (Boundary (SolrExpr b))
  | forall b. EBoost (SolrExpr b) Float

instance SolrExprSYM SolrExpr where
  num    = ENum
  true   = ETrue
  false  = EFalse
  word   = EWord
  wild   = EWild
  regex  = ERegex
  phrase = EPhrase
  (~:)   = EFuzz
  to     = ETo
  (^:)   = EBoost


-- | An untyped Solr query.
data SolrQuery
  = forall a. QDefaultField (SolrExpr a)
  | forall a. QField Text (SolrExpr a)
  | QAnd SolrQuery SolrQuery
  | QOr SolrQuery SolrQuery
  | QNot SolrQuery SolrQuery
  | QScore SolrQuery Float
  | QNeg SolrQuery
  | QParams [Param SolrQuery] SolrQuery

instance SolrQuerySYM SolrExpr SolrQuery where
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

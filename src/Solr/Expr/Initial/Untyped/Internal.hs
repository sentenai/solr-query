{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Solr.Expr.Initial.Untyped.Internal where

import Solr.Internal.Class.Expr
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

deriving instance Show (SolrExpr a)

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

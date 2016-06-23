{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}

module Solr.Expr.Initial.Untyped
  ( -- * Expression type
    SolrExpr(..)
    -- * Expression construction
  , num
  , true
  , false
  , word
  , wild
  , regex
  , phrase
  , (~:)
  , fuzz
  , fuzzy
  , to
  , incl
  , excl
  , star
  , gt
  , gte
  , lt
  , lte
  , (^:)
  , boost
  ) where

import Solr.Internal.Class
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

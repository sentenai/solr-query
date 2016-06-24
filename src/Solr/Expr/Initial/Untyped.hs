module Solr.Expr.Initial.Untyped
  ( -- * Expression type
    SolrExpr(..)
    -- * Type checking
  , typeCheckSolrExpr
    -- * Re-exports
  , module Solr.Internal.Class.Expr
  ) where

import Solr.Expr.Initial.Untyped.Internal
import Solr.Internal.Class.Expr

import qualified Solr.Expr.Initial.Typed as Typed


-- | Type check an untyped Solr expression. Returns whether or not the
-- expression is well-typed.
typeCheckSolrExpr :: SolrExpr a -> Bool
typeCheckSolrExpr e = Typed.typeCheckSolrExpr e (maybe False (const True))

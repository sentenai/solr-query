module Solr.Query.Initial.Untyped
  ( -- * Query type
    SolrQuery(..)
    -- * Type checking
  , Solr.Query.Initial.Untyped.typeCheckSolrQuery
    -- * Re-exports
  , module Solr.Internal.Class.Query
  , module Solr.Query.Param
  ) where

import Solr.Internal.Class.Query
import Solr.Query.Initial.Internal
import Solr.Query.Param

import qualified Solr.Expr.Initial.Untyped as Untyped


-- | Type check an untyped Solr query. Returns whether or not the query is
-- well-typed.
typeCheckSolrQuery :: SolrQuery Untyped.SolrExpr -> Bool
typeCheckSolrQuery e =
  maybe False (const True) (Solr.Query.Initial.Internal.typeCheckSolrQuery e)

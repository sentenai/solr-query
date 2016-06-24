module Solr.Query.Initial.Typed
  ( -- * Query type
    SolrQuery(..)
    -- * Type checking
  , typeCheckSolrQuery
    -- * Re-exports
  , module Solr.Internal.Class.Query
  , module Solr.Query.Param
  ) where

import Solr.Internal.Class.Query
import Solr.Query.Initial.Internal
import Solr.Query.Param

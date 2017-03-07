module Solr.Query
  ( -- * Query types
    Query
  , InterpretQuery(..)
    -- * Query parameters
  , Param
  , Cache(..)
  , Cost
  , fl
  , fq
  , rows
  , sortAsc
  , sortDesc
  , start
  ) where

import Solr.Query.Internal (Query, InterpretQuery(..))
import Solr.Query.Param

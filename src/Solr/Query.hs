module Solr.Query
  ( -- * Query class
    Query
  , LocalParams
  , should
  , must
  , mustNot
  , filt
  , SomeQuery
  , someQuery
    -- * Compiling a 'Query'
  , compile
    -- ** 'Query' params
  , Param
  , fl
  , fq
  , q
  , rows
  , sortAsc
  , sortDesc
  , start
    -- * Re-exports
  , (&)
  , def
  ) where

import Solr.Prelude

import Builder
import Solr.Query.Internal
import Solr.Query.Param

import qualified Data.Text.Lazy

-- | Compile a list of 'Param' to a lazy 'Data.Text.Lazy.Text'.
compile :: [Param] -> Data.Text.Lazy.Text
compile = freeze . intersperse '&' . map compileParam

module Solr.Query
  ( -- * Query class
    Query
  , LocalParams
  , (&&:)
  , (||:)
  , (-:)
  , SomeQuery
  , someQuery
    -- * Compiling a 'Query'
  , compile
    -- ** 'Query' params
  , Param
  , fl
  , fq
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
import Solr.Query.Internal.Internal
import Solr.Query.Param

import qualified Data.Text.Lazy

-- | Compile a 'Query' a lazy 'Data.Text.Lazy.Text'.
compile
  :: Query query => [Param] -> LocalParams query -> query -> Data.Text.Lazy.Text
compile params locals query =
  freeze (intersperse '&'
    ("q=" <> compileQuery locals query : map f params))
 where
  f :: Param -> Builder
  f = (\(x, y) -> x <> char '=' <> y) . compileParam

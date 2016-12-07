-- | Query parameters.

module Solr.Param
  ( -- * Query parameters
    Param(..)
  , IsFilterQuery
  , fq
  , rows
  , start
  ) where

import Solr.LocalParam.Internal
import Solr.Query.Class

-- $setup
-- >>> import Data.Semigroup
-- >>> import Solr.Query

-- A convenient constraint alias for what it means to be a filter query: it
-- supports a specific set of 'LocalParam's.
type IsFilterQuery query
  = (HasLocalParamCache query, HasLocalParamCost query, HasLocalParamDf query,
      HasLocalParamOp query)

data Param
  = ParamFq
      (forall query. IsFilterQuery query => [LocalParam query])
      (forall expr query. QuerySYM expr query => query expr)
  | ParamRows Int
  | ParamStart Int

-- | The @\'fq\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: word "bar" :: Query Expr
-- >>> let filterQuery = "baz" =: gte (int 10) :: QuerySYM expr query => query expr
-- >>> compile [fq [] filterQuery] [] query
-- "fq=baz:[10 TO *]&q=foo:bar"
fq :: (forall query. IsFilterQuery query => [LocalParam query])
   -> (forall expr query. QuerySYM expr query => query expr)
   -> Param
fq = ParamFq

-- | The @\'rows\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: word "bar" :: Query Expr
-- >>> compile [rows 5] [] query
-- "rows=5&q=foo:bar"
rows :: Int -> Param
rows = ParamRows

-- | The @\'start\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: word "bar" :: Query Expr
-- >>> compile [start 10] [] query
-- "start=10&q=foo:bar"
start :: Int -> Param
start = ParamStart

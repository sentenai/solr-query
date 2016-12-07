-- | This is the simplest interpretation of the Solr query language as a lazy
-- 'Text'.
--
-- Not all type-correct expressions using the Solr DSL result in well-formed
-- queries. For example,
--
-- >>> let query = (("foo" =: word "bar") ^=: 1.0) ^=: 2.0 :: QuerySYM expr query => query expr
-- >>> compile [] (query :: Query Expr)
-- "q=foo:bar^=1.0^=2.0"
--
-- For this reason, you may want to first interpret a query using
-- "Solr.Query.Initial", manually fix up the AST
-- (perhaps with 'Solr.Query.Initial.factor'), and then reinterpret it as the
-- lazy 'Text' version using 'Solr.Query.Initial.reinterpret':
--
-- >>> import Solr.Query.Initial (factor, reinterpret)
-- >>> compile [] (reinterpret (factor query) :: Query Expr)
-- "q=foo:bar^=2.0"

module Solr.Query
  ( -- * Query
    Query
  , compile
    -- * Re-exports
  , module Solr.Expr
  , module Solr.Expr.Class
  , module Solr.Query.Class
  , module Solr.Param
  ) where

import Solr.Expr
import Solr.Expr.Class
import Solr.Param (Param, df, opAnd, opOr, rows, start)
import Solr.Query.Class
import Solr.Query.Internal

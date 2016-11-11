module Solr.FilterQuery
  ( -- * Filter query
    FilterQuery
  , compile
    -- * Re-exports
  , module Solr.Expr.Class
  , module Solr.Query.Class
  , module Solr.Param
  ) where

import Builder
import Solr.Expr.Class
import Solr.Expr.Internal
import Solr.Param
import Solr.Param.Internal
import Solr.Query.Class
import Solr.Query.Internal hiding (compile)

import Data.Semigroup (Semigroup(..))
import Data.Text.Lazy (Text)

-- | A Solr filter query. This is like 'Query', but with different local
-- parameters available. All functions polymorphic over 'QuerySYM' will work
-- with both.
newtype FilterQuery expr = FQ (Query expr)
  deriving Semigroup

instance QuerySYM Expr FilterQuery where
  defaultField e = FQ (defaultField e)

  f =: e = FQ (f =: e)

  FQ q1 &&: FQ q2 = FQ (q1 &&: q2)

  FQ q1 ||: FQ q2 = FQ (q1 ||: q2)

  FQ q1 -: FQ q2 = FQ (q1 -: q2)

  FQ q ^=: n = FQ (q ^=: n)

  neg (FQ q) = FQ (neg q)

instance HasParamCache        FilterQuery
instance HasParamCost         FilterQuery
instance HasParamDefaultField FilterQuery
instance HasParamOp           FilterQuery
instance HasParamRows         FilterQuery
instance HasParamStart        FilterQuery

-- | Compile a 'FilterQuery' to a lazy 'Text'.
compile :: [Param FilterQuery] -> FilterQuery expr -> Text
compile params (FQ (Q query)) =
  case params of
    [] -> freeze ("fq=" <> query)
    _ -> freeze ("fq={!" <> spaces (map compileParam params) <> "}" <> query)

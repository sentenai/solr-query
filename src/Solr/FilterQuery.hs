module Solr.FilterQuery where

import Builder
import Solr.Expr.Internal
import Solr.LocalParam
import Solr.LocalParam.Internal
import Solr.Query.Class
import Solr.Query.Internal hiding (compile)

import Data.Semigroup (Semigroup(..))
import Data.Text.Lazy (Text)

-- | A Solr filter query. This is like 'Query', but with more 'LocalParam's
-- available. All functions polymorphic over 'QuerySYM' will work with both.
newtype FilterQuery expr = FQ (Query expr)
  deriving Semigroup

instance QuerySYM Expr FilterQuery where
  defaultField e = FQ (defaultField e)

  f =: e = FQ (f =: e)

  FQ q1 &&: FQ q2 = FQ (q1 &&: q2)

  FQ q1 ||: FQ q2 = FQ (q1 ||: q2)

  FQ q1 -: FQ q2 = FQ (q1 -: q2)

  FQ q ^=: n = FQ (q ^=: n)

instance HasLocalParamCache FilterQuery
instance HasLocalParamCost  FilterQuery
instance HasLocalParamDf    FilterQuery
instance HasLocalParamOp    FilterQuery

-- | Compile a 'FilterQuery' to a lazy 'Text'.
compile :: [LocalParam FilterQuery] -> FilterQuery expr -> Text
compile locals (FQ (Q query)) = freeze ("fq=" <> locals' <> query)
 where
  locals' =
    case locals of
      [] -> mempty
      _  -> "{!" <> intersperse ' ' (map compileLocalParam locals) <> "}"

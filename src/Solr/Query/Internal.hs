module Solr.Query.Internal where

import Builder
import Solr.Expr.Internal
import Solr.Param.Internal
import Solr.Query.Class
import Solr.Type

import Data.Semigroup (Semigroup(..))
import Data.Text.Lazy (Text)

-- | A Solr query.
newtype Query (expr :: SolrType -> *) = Q { unQ :: Builder }

-- | Appending Solr queries simply puts a space between them. To Solr, this is
-- equivalent to combining them with \'OR\'. However, this behavior can be
-- adjusted on a per-query basis using 'paramOp'.
--
-- Due to limited precedence options, ('<>') will typically require parens
-- around its arguments.
instance Semigroup (Query expr) where
  q1 <> q2 = Q (unQ q1 <> " " <> unQ q2)

instance QuerySYM Expr Query where
  defaultField e = Q (unE e)

  f =: e = Q (thaw' f <> char ':' <> unE e)

  Q q1 &&: Q q2 = Q (parens (q1 <> " AND " <> q2))

  Q q1 ||: Q q2 = Q (parens (q1 <> " OR " <> q2))

  Q q1 -: Q q2 = Q (parens (q1 <> " NOT " <> q2))

  Q q ^=: n = Q (q <> "^=" <> bshow n)

  neg (Q q) = Q (char '-' <> q)

instance HasParamDefaultField Query
instance HasParamOp           Query
instance HasParamRows         Query
instance HasParamStart        Query

compileParam :: Param query -> Builder
compileParam = \case
  ParamCache b        -> "cache=" <> if b then "true" else "false"
  ParamCost n         -> "cost=" <> bshow n
  ParamDefaultField v -> "df=" <> thaw' v
  ParamOpAnd          -> "q.op=AND"
  ParamOpOr           -> "q.op=OR"
  ParamRows n         -> "rows=" <> bshow n
  ParamStart n        -> "start=" <> bshow n

-- | Compile a 'Query' to a lazy 'Text'.
--
-- Note that the DSL admits many ways to create an invalid Solr query (e.g.
-- multiple 'neg's); that is, if it compiles, it doesn't necessarily work.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: phrase ["bar", "baz"] ~: 5 &&: defaultField (regex "wh?t") :: Query Expr
-- >>> compile [paramDefaultField "body"] query
-- "q={!df=body}(foo:\"bar baz\"~5 AND /wh?t/)"
compile
  :: [Param Query] -> Query expr -> Text
compile params (Q query) =
  case params of
    [] -> freeze ("q=" <> query)
    _  -> freeze ("q={!" <> spaces (map compileParam params) <> "}" <> query)

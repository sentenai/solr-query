{-# language CPP                  #-}
{-# language UndecidableInstances #-}

module Solr.Query.Internal where

import Builder
import Solr.Expr.Internal
import Solr.LocalParam.Internal
import Solr.Param
import Solr.Query.Class
import Solr.Type

import Data.Semigroup (Semigroup(..))
import Data.Text.Lazy (Text)

#if MIN_VERSION_base(4,9,0)
import GHC.TypeLits (TypeError, ErrorMessage(..))
#endif

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

instance HasLocalParamDf Query
instance HasLocalParamOp Query
#if MIN_VERSION_base(4,9,0)
instance TypeError ('Text "Query cannot have a 'cache' local parameter") => HasLocalParamCache Query
instance TypeError ('Text "Query cannot have a 'cost' local parameter")  => HasLocalParamCost  Query
#endif

-- | Compile a 'Query' to a lazy 'Text'.
--
-- Note that the DSL admits many ways to create an invalid Solr query (e.g.
-- multiple 'neg's); that is, if it compiles, it doesn't necessarily work.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: phrase ["bar", "baz"] ~: 5 &&: defaultField (regex "wh?t") :: Query Expr
-- >>> compile [] [df "body"] query
-- "q={!df=body}(foo:\"bar baz\"~5 AND /wh?t/)"
compile
  :: [Param] -> [LocalParam Query] -> Query expr -> Text
compile params locals (Q query) = freeze (params' <> "q=" <> locals' <> query)
 where
  params' = intersperse '&' (map compileParam params)
  locals' =
    case locals of
      [] -> mempty
      _  -> "{!" <> intersperse ' ' (map compileLocalParam locals) <> "}"

compileParam :: Param -> Builder
compileParam = \case
  ParamRows n  -> "rows=" <> bshow n
  ParamStart n -> "start=" <> bshow n

compileLocalParam :: LocalParam query -> Builder
compileLocalParam = \case
  LocalParamCache b -> "cache=" <> if b then "true" else "false"
  LocalParamCost n  -> "cost=" <> bshow n
  LocalParamDf v    -> "df=" <> thaw' v
  LocalParamOpAnd   -> "q.op=AND"
  LocalParamOpOr    -> "q.op=OR"

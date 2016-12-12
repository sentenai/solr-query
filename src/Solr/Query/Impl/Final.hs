{-# language CPP #-}

#if MIN_VERSION_base(4,9,0)
{-# options_ghc -fno-warn-redundant-constraints #-}
#endif

module Solr.Query.Impl.Final where

import Builder
import Solr.Prelude
import Solr.Expr.Impl.Final
import Solr.Expr.Type
import Solr.Query.Class
import Solr.Query.LocalParam
import Solr.Query.Param

import qualified Data.Text.Lazy

-- $setup
-- >>> import Solr.Expr.Class

-- 'Builder' interpretation of 'QuerySYM'.
newtype Q (expr :: ExprTy -> *) = Q { unQ :: Builder }

instance Semigroup (Q expr) where
  q1 <> q2 = Q (unQ q1 <> char ' ' <> unQ q2)

instance QuerySYM E Q where
  defaultField e = Q (unE e)

  f =: e = Q (thaw' f <> char ':' <> unE e)

  Q q1 &&: Q q2 = Q (parens (q1 <> " AND " <> q2))

  Q q1 ||: Q q2 = Q (parens (q1 <> " OR " <> q2))

  Q q1 -: Q q2 = Q (parens (q1 <> " NOT " <> q2))

  Q q ^=: n = Q (q <> "^=" <> bshow n)

-- 'Builder' interpretation of 'QuerySYM'. This is like 'Q', but with more
-- 'LocalParam's available.
newtype FilterQuery expr = FQ (Q expr)
  deriving (Semigroup, QuerySYM E)

-- | Compile a 'Query' to a lazy 'Data.Text.Lazy.Text'.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: phrase ["bar", "baz"] ~: 5 &&: defaultField (regex "wh?t")
-- >>> compile [] [df "body"] query
-- "q={!df=body}(foo:\"bar baz\"~5 AND /wh?t/)"
compile :: [Param] -> [LocalParam 'QueryLocalParam] -> Query -> Data.Text.Lazy.Text
compile params locals query =
  freeze (compileParams params <> "q=" <> compileQuery locals query)

compileQuery
  :: [LocalParam ty] -> Q E -> Builder
compileQuery locals (Q query) = compileLocalParams locals <> query

compileParams :: [Param] -> Builder
compileParams = foldr (\p b -> go p <> char '&' <> b) mempty
 where
  go = \case
    ParamFl s                 -> "fl="    <> thaw' s
    ParamFq locals (FQ query) -> "fq="    <> compileQuery locals query
    ParamRows n               -> "rows="  <> bshow n
    ParamSortAsc s            -> "sort="  <> thaw' s <> " asc"
    ParamSortDesc s           -> "sort="  <> thaw' s <> " desc"
    ParamStart n              -> "start=" <> bshow n

compileLocalParams :: [LocalParam ty] -> Builder
compileLocalParams [] = mempty
compileLocalParams ps = "{!" <> intersperse ' ' (map go ps) <> "}"
 where
  go = \case
    LocalParamCache b -> "cache=" <> if b then "true" else "false"
    LocalParamCost n  -> "cost=" <> bshow n
    LocalParamDf v    -> "df=" <> thaw' v
    LocalParamOpAnd   -> "q.op=AND"
    LocalParamOpOr    -> "q.op=OR"

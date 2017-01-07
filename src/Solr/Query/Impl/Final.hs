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

-- | Compile a 'Query' with 'Param's and 'LocalParam's to a lazy
-- 'Data.Text.Lazy.Text'.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: phrase ["bar", "baz"] ~: 5 &&: defaultField (regex "wh?t")
-- >>> compile [] [df "body"] query
-- "q={!df=body}(foo:\"bar baz\"~5 AND /wh?t/)"
compile :: [Param] -> [LocalParam 'QueryLocalParam] -> Query -> Data.Text.Lazy.Text
compile params locals query =
  freeze (compileParams params <> "q=" <> compileLocalParams locals <>
           compileQuery query)

compileParams :: [Param] -> Builder
compileParams = foldr (\p b -> compileParam p <> char '&' <> b) mempty

-- | Compile a 'Param' to a 'Builder'. Usually 'compile' is more convenient.
--
-- ==== __Examples__
--
-- >>> compileParam (rows 5)
-- "rows=5"
compileParam :: Param -> Builder
compileParam = \case
  ParamFl s -> "fl=" <> thaw' s
  ParamFq locals (FQ (Q query :: Q E)) ->
    "fq=" <> compileLocalParams locals <> query
  ParamRows n -> "rows=" <> bshow n
  ParamSortAsc s -> "sort=" <> thaw' s <> " asc"
  ParamSortDesc s -> "sort=" <> thaw' s <> " desc"
  ParamStart n -> "start=" <> bshow n

compileLocalParams :: [LocalParam ty] -> Builder
compileLocalParams [] = mempty
compileLocalParams ps = "{!" <> intersperse ' ' (map compileLocalParam ps) <> "}"

-- | Compile a 'LocalParam' to a 'Builder'. Usually 'compile' is more
-- convenient.
--
-- ==== __Examples__
--
-- >>> compileLocalParam (cache True)
-- "cache=true"
compileLocalParam :: LocalParam ty -> Builder
compileLocalParam = \case
  LocalParamCache b -> "cache=" <> if b then "true" else "false"
  LocalParamCost n  -> "cost=" <> bshow n
  LocalParamDf v    -> "df=" <> thaw' v
  LocalParamOpAnd   -> "q.op=AND"
  LocalParamOpOr    -> "q.op=OR"

-- | Compile a 'Query' to a 'Builder'. Usually 'compile' is more convenient.
--
-- ==== __Examples__
--
-- >>> compileQuery ("foo" =: word "bar")
-- "foo:bar"
compileQuery :: Query -> Builder
compileQuery (Q query :: Q E) = query

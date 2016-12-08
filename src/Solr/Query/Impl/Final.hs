{-# language CPP                  #-}
{-# language UndecidableInstances #-}

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

#if MIN_VERSION_base(4,9,0)
import GHC.TypeLits (TypeError, ErrorMessage(..))
#endif

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

instance HasLocalParamDf Q
instance HasLocalParamOp Q
#if MIN_VERSION_base(4,9,0)
instance TypeError ('Text "Query cannot have a 'cache' local parameter") => HasLocalParamCache Q
instance TypeError ('Text "Query cannot have a 'cost' local parameter")  => HasLocalParamCost  Q
#endif

-- 'Builder' interpretation of 'QuerySYM'. This is like 'Q', but with more
-- 'LocalParam's available.
newtype FilterQuery expr = FQ (Q expr)
  deriving (Semigroup, QuerySYM E)

instance HasLocalParamCache FilterQuery
instance HasLocalParamCost  FilterQuery
instance HasLocalParamDf    FilterQuery
instance HasLocalParamOp    FilterQuery

-- | Compile a 'Query' to a lazy 'Data.Text.Lazy.Text'.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: phrase ["bar", "baz"] ~: 5 &&: defaultField (regex "wh?t")
-- >>> compile [] [df "body"] query
-- "q={!df=body}(foo:\"bar baz\"~5 AND /wh?t/)"
compile :: [Param] -> QueryLocalParams -> Query -> Data.Text.Lazy.Text
compile params (locals :: [LocalParam Q]) (Q query :: Q E) =
  freeze (compileParams params <> "q=" <> compileLocalParams locals <> query)

compileFilterQuery :: [LocalParam FilterQuery] -> FilterQuery E -> Builder
compileFilterQuery locals (FQ (Q query)) = compileLocalParams locals <> query

compileParams :: [Param] -> Builder
compileParams = foldr (\p b -> go p <> char '&' <> b) mempty
 where
  go = \case
    ParamFl s -> "fl=" <> thaw' s
    ParamFq locals query -> "fq=" <> compileFilterQuery locals query
    ParamRows n  -> "rows=" <> bshow n
    ParamStart n -> "start=" <> bshow n

compileLocalParams :: [LocalParam query] -> Builder
compileLocalParams [] = mempty
compileLocalParams ps = "{!" <> intersperse ' ' (map go ps) <> "}"
 where
  go = \case
    LocalParamCache b -> "cache=" <> if b then "true" else "false"
    LocalParamCost n  -> "cost=" <> bshow n
    LocalParamDf v    -> "df=" <> thaw' v
    LocalParamOpAnd   -> "q.op=AND"
    LocalParamOpOr    -> "q.op=OR"

{-# language CPP                  #-}
{-# language ScopedTypeVariables  #-}
{-# options_ghc -fno-warn-orphans #-}

#if MIN_VERSION_base(4,9,0)
{-# options_ghc -fno-warn-redundant-constraints #-}
#endif

module Solr.Query.Lucene.Impl.Final where

import Builder
import Solr.Prelude
import Solr.Query (InterpretQuery(interpretParams, interpretQuery), Query)
import Solr.Query.Lucene.Class
import Solr.Query.Lucene.Expr.Impl.Final
import Solr.Query.Param

import Data.Proxy (Proxy(Proxy))

import qualified Data.Text.Lazy

-- $setup
-- >>> import Solr.Query.Lucene.Expr.Class

-- 'Builder' interpretation of 'LuceneQuerySYM'.
newtype Q = Q { unQ :: Builder }

instance Semigroup Q where
  q1 <> q2 = Q (unQ q1 <> char ' ' <> unQ q2)

instance LuceneQuerySYM Q where
  defaultField e = Q (unE e)

  f =: e = Q (thaw' f <> char ':' <> unE e)

  Q q1 &&: Q q2 = Q (parens (q1 <> " AND " <> q2))

  Q q1 ||: Q q2 = Q (parens (q1 <> " OR " <> q2))

  Q q1 -: Q q2 = Q (parens (q1 <> " NOT " <> q2))

  Q q ^=: n = Q (q <> "^=" <> bshow n)

instance InterpretQuery LuceneQuerySYM Builder where
  interpretParams :: [LuceneQueryParam] -> Builder
  interpretParams [] = "lucene"
  interpretParams ps =
    "lucene " <> intersperse ' ' (map compileLocalParam ps)

  interpretQuery :: Proxy LuceneQuerySYM -> LuceneQuery -> Builder
  interpretQuery _ = compileQuery

-- | Compile a 'LuceneQuery' with 'Param's and 'LuceneQueryParam's to a lazy
-- 'Data.Text.Lazy.Text'.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: phrase ["bar", "baz"] ~: 5 &&: defaultField (regex "wh?t")
-- >>> compile [] [df "body"] query
-- "q={!lucene df=body}(foo:\"bar baz\"~5 AND /wh?t/)"
compile
  :: [Param Builder] -> [LuceneQueryParam] -> LuceneQuery
  -> Data.Text.Lazy.Text
compile params locals query =
  freeze
    (compileParams params <> "q={!" <> interpretParams locals
      <> char '}' <> interpretQuery (Proxy :: Proxy LuceneQuerySYM) query)

compileParams :: [Param Builder] -> Builder
compileParams = foldr (\p b -> compileParam p <> char '&' <> b) mempty

-- | Compile a 'Param' to a 'Builder'. Usually 'compile' is more convenient.
--
-- ==== __Examples__
--
-- >>> compileParam (rows 5)
-- "rows=5"
compileParam :: Param Builder -> Builder
compileParam = \case
  ParamFl s -> "fl=" <> thaw' s
  ParamFq cache cost (locals :: [LocalParam sym]) (query :: Query sym) ->
    "fq={!" <> interpretParams locals <> " cache=" <> compileCache cache <>
      compileCost cost <> char '}' <> interpretQuery (Proxy :: Proxy sym) query
  ParamRows n -> "rows=" <> bshow n
  ParamSortAsc s -> "sort=" <> thaw' s <> " asc"
  ParamSortDesc s -> "sort=" <> thaw' s <> " desc"
  ParamStart n -> "start=" <> bshow n
 where
  compileCache :: Cache -> Builder
  compileCache Cache = "true"
  compileCache DontCache = "false"

  compileCost :: Maybe Cost -> Builder
  compileCost = maybe mempty (\cost -> " cost=" <> bshow cost)

-- | Compile a 'LuceneQueryParam' to a 'Builder'. Usually 'compile' is more
-- convenient.
--
-- ==== __Examples__
--
-- >>> compileLocalParam opAnd
-- "q.op=AND"
compileLocalParam :: LuceneQueryParam -> Builder
compileLocalParam = \case
  Df v  -> "df=" <> thaw' v
  OpAnd -> "q.op=AND"
  OpOr  -> "q.op=OR"

-- | Compile a 'LuceneQuery' to a 'Builder'. Usually 'compile' is more
-- convenient.
--
-- ==== __Examples__
--
-- >>> compileQuery ("foo" =: word "bar")
-- "foo:bar"
compileQuery :: LuceneQuery -> Builder
compileQuery (Q query) = query

-- | Solr query construction and compilation. This is the simplest
-- interpretation of the Solr query language as a lazy 'LByteString.ByteString'.
--
-- This module re-exports the expression and query languages from
-- "Solr.Internal.Class.Query" /inline/, for ease of browsing the haddocks.
-- Other modules simply re-export those modules whole.
--
-- Not all type-correct expressions using the Solr DSL result in well-formed
-- queries. For example,
--
-- >>> let query = (("foo" =: word "bar") ^=: 1.0) ^=: 2.0 :: SolrQuerySYM expr query => query expr
-- >>> compileSolrQuery [] (query :: SolrQuery SolrExpr)
-- "q=foo:bar^=1.0^=2.0"
--
-- For this reason, you may want to first interpret a query using
-- "Solr.Query.Initial", manually fix up the AST
-- (perhaps with 'Solr.Query.Initial.factorSolrQuery'), and then reinterpret it as the
-- lazy 'Data.ByteString.Lazy.ByteString' version using
-- 'Solr.Query.Initial.reinterpretSolrQuery':
--
-- >>> import Solr.Query.Initial (factorSolrQuery, reinterpretSolrQuery)
-- >>> compileSolrQuery [] (reinterpretSolrQuery (factorSolrQuery query) :: SolrQuery SolrExpr)
-- "q=foo:bar^=2.0"

module Solr.Query
  (
  -- * Query type
    SolrQuery
  , SolrFilterQuery
  -- * Query construction
  , defaultField
  , (=:)
  , field
  , (&&:)
  , Solr.Internal.Class.Query.and
  , (||:)
  , Solr.Internal.Class.Query.or
  , (-:)
  , Solr.Internal.Class.Query.not
  , (^=:)
  , score
  , neg
    -- * Expression type
  , SolrExpr
    -- * Expression construction
  , num
  , true
  , false
  , word
  , wild
  , regex
  , phrase
  , (~:)
  , fuzz
  , fuzzy
  , to
  , incl
  , excl
  , star
  , gt
  , gte
  , lt
  , lte
  , (^:)
  , boost
  -- * Local parameters
  , Param
  , paramCache
  , paramCost
  , paramDefaultField
  , paramOpAnd
  , paramOpOr
  -- * Query compilation
  , compileSolrQuery
  , compileSolrFilterQuery
  ) where

import Builder                   (Builder)
import Solr.Expr.Internal
import Solr.Internal.Class.Query
import Solr.Query.Param.Internal
import Solr.Type

import qualified Builder

import Data.Semigroup (Semigroup(..))

import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text.Encoding   as Text


-- | A Solr query.
newtype SolrQuery (expr :: SolrType -> *) = Query { unQuery :: Builder }

-- | Appending Solr queries simply puts a space between them. To Solr, this is
-- equivalent to combining them with \'OR\'. However, this behavior can be
-- adjusted on a per-query basis using 'paramOp'.
--
-- Due to limited precedence options, ('<>') will typically require parens
-- around its arguments.
instance Semigroup (SolrQuery expr) where
  q1 <> q2 = Query (unQuery q1 <> " " <> unQuery q2)

instance SolrQuerySYM SolrExpr SolrQuery where
  defaultField e = Query (unExpr e)

  f =: e = Query (Text.encodeUtf8Builder f <> ":" <> unExpr e)

  q1 &&: q2 = Query ("(" <> unQuery q1 <> " AND " <> unQuery q2 <> ")")

  q1 ||: q2 = Query ("(" <> unQuery q1 <> " OR " <> unQuery q2 <> ")")

  q1 -: q2 = Query ("(" <> unQuery q1 <> " NOT " <> unQuery q2 <> ")")

  q ^=: n = Query (unQuery q <> "^=" <> Builder.show n)

  neg q = Query ("-" <> unQuery q)

instance HasParamDefaultField SolrQuery
instance HasParamOp           SolrQuery
instance HasParamRows         SolrQuery
instance HasParamStart        SolrQuery


-- | A Solr filter query. This is like 'SolrQuery', but with different local
-- parameters available. All functions polymorphic over 'SolrQuerySYM' will work
-- with both.
newtype SolrFilterQuery expr = FQuery { unFQuery :: SolrQuery expr }
  deriving Semigroup

instance SolrQuerySYM SolrExpr SolrFilterQuery where
  defaultField e = FQuery (defaultField e)

  f =: e = FQuery (f =: e)

  q1 &&: q2 = FQuery (unFQuery q1 &&: unFQuery q2)

  q1 ||: q2 = FQuery (unFQuery q1 ||: unFQuery q2)

  q1 -: q2 = FQuery (unFQuery q1 -: unFQuery q2)

  q ^=: n = FQuery (unFQuery q ^=: n)

  neg q = FQuery (neg (unFQuery q))

instance HasParamCache        SolrFilterQuery
instance HasParamCost         SolrFilterQuery
instance HasParamDefaultField SolrFilterQuery
instance HasParamOp           SolrFilterQuery
instance HasParamRows         SolrFilterQuery
instance HasParamStart        SolrFilterQuery


compileParam :: Param query -> Builder
compileParam = \case
  ParamCache b        -> "cache=" <> if b then "true" else "false"
  ParamCost n         -> "cost=" <> Builder.show n
  ParamDefaultField v -> "df=" <> Text.encodeUtf8Builder v
  ParamOpAnd          -> "q.op=AND"
  ParamOpOr           -> "q.op=OR"
  ParamRows n         -> "rows=" <> Builder.show n
  ParamStart n        -> "start=" <> Builder.show n

-- | Compile a 'SolrQuery' to a lazy 'ByteString'.
--
-- Note that the DSL admits many ways to create an invalid Solr query; that is,
-- if it compiles, it doesn't necessarily work. For example, multiple 'neg's on
-- a query, multiple 'params', etc.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: phrase ["bar", "baz"] ~: 5 &&: defaultField (regex "wh?t") :: SolrQuery SolrExpr
-- >>> compileSolrQuery [paramDefaultField "body"] query
-- "q={!df=body}(foo:\"bar baz\"~5 AND /wh?t/)"
compileSolrQuery
  :: [Param SolrQuery] -> SolrQuery expr -> LByteString.ByteString
compileSolrQuery params (Query query) =
  case params of
    [] -> Builder.toLazyByteString ("q=" <> query)
    _  ->
      Builder.toLazyByteString
        ("q={!" <> Builder.spaces (map compileParam params) <> "}" <> query)

-- | Compile a 'SolrFilterQuery' to a lazy 'ByteString'.
compileSolrFilterQuery
  :: [Param SolrFilterQuery] -> SolrFilterQuery expr -> LByteString.ByteString
compileSolrFilterQuery params (FQuery (Query query)) =
  case params of
    [] -> Builder.toLazyByteString ("fq=" <> query)
    _ ->
      Builder.toLazyByteString
        ("fq={!" <> Builder.spaces (map compileParam params) <> "}" <> query)

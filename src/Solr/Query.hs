-- | Solr query construction and compilation. This is the simplest
-- interpretation of the Solr query language as a lazy 'Text'.
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
-- lazy 'Text' version using 'Solr.Query.Initial.reinterpretSolrQuery':
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
  , Solr.Query.Class.and
  , (||:)
  , Solr.Query.Class.or
  , (-:)
  , Solr.Query.Class.not
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
  , Boundary
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
  , paramRows
  , paramStart
  -- * Query compilation
  , compileSolrQuery
  , compileSolrFilterQuery
  ) where

import Builder
import Solr.Expr.Internal
import Solr.Param.Internal
import Solr.Query.Class
import Solr.Type

import Data.Semigroup (Semigroup(..))
import Data.Text.Lazy (Text)


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

  f =: e = Query (thaw' f <> char ':' <> unExpr e)

  Query q1 &&: Query q2 = Query (parens (q1 <> " AND " <> q2))

  Query q1 ||: Query q2 = Query (parens (q1 <> " OR " <> q2))

  Query q1 -: Query q2 = Query (parens (q1 <> " NOT " <> q2))

  Query q ^=: n = Query (q <> "^=" <> bshow n)

  neg (Query q) = Query (char '-' <> q)

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
  ParamCost n         -> "cost=" <> bshow n
  ParamDefaultField v -> "df=" <> thaw' v
  ParamOpAnd          -> "q.op=AND"
  ParamOpOr           -> "q.op=OR"
  ParamRows n         -> "rows=" <> bshow n
  ParamStart n        -> "start=" <> bshow n

-- | Compile a 'SolrQuery' to a lazy 'Text'.
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
  :: [Param SolrQuery] -> SolrQuery expr -> Text
compileSolrQuery params (Query query) =
  case params of
    [] -> freeze ("q=" <> query)
    _  -> freeze ("q={!" <> spaces (map compileParam params) <> "}" <> query)

-- | Compile a 'SolrFilterQuery' to a lazy 'Text'.
compileSolrFilterQuery
  :: [Param SolrFilterQuery] -> SolrFilterQuery expr -> Text
compileSolrFilterQuery params (FQuery (Query query)) =
  case params of
    [] -> freeze ("fq=" <> query)
    _ -> freeze ("fq={!" <> spaces (map compileParam params) <> "}" <> query)

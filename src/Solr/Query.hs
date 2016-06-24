{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Solr query construction and compilation. This is the "simplest"
-- interpretation of the Solr query language as a lazy 'LByteString.ByteString'.
--
-- This module re-exports the expression and query languages from
-- "Solr.Internal.Class.Query" /inline/, for ease of browsing the haddocks.
-- Other modules simply re-export those modules whole.

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
  , params
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
  , ParamKey
  , Param(..)
  , (.=)
  , HasParamDefaultField(..)
  , HasParamOp(..)
  , HasParamCache(..)
  , HasParamCost(..)
  -- * Query compilation
  , compileSolrQuery
  , compileSolrFilterQuery
  ) where

import Builder                   (Builder)
import Solr.Expr.Internal
import Solr.Internal.Class.Query
import Solr.Query.Param
import Solr.Type

import qualified Builder

import Data.Semigroup (Semigroup(..))
import Data.Text      (Text)

import qualified Data.ByteString.Lazy    as LByteString
import qualified Data.Text.Encoding      as Text
import qualified Data.Text.Lazy          as LText
import qualified Data.Text.Lazy.Encoding as LText


-- | A Solr query.
newtype SolrQuery (expr :: SolrType -> *) = Query { unQuery :: Builder }

-- | For debugging. Calls 'compileSolrQuery'.
instance Show (SolrQuery expr) where
  show = showb . compileSolrQuery

-- | Appending Solr queries simply puts a space between them. To Solr, this is
-- equivalent to combining them with \'OR\'. However, this behavior can be
-- adjusted on a per-query basis using 'paramOp'.
--
-- Due to limited precedence options, ('<>') will typically require parens
-- around its arguments.
instance Semigroup (SolrQuery expr) where
  q1 <> q2 = Query (unQuery q1 <> " " <> unQuery q2)

instance SolrQuerySYM SolrExpr SolrQuery where
  data ParamKey SolrQuery a where
    SolrQueryDefaultField :: ParamKey SolrQuery Text
    SolrQueryOp           :: ParamKey SolrQuery Text

  defaultField e = Query (unExpr e)

  f =: e = Query (Text.encodeUtf8Builder f <> ":" <> unExpr e)

  q1 &&: q2 = Query ("(" <> unQuery q1 <> " AND " <> unQuery q2 <> ")")

  q1 ||: q2 = Query ("(" <> unQuery q1 <> " OR " <> unQuery q2 <> ")")

  q1 -: q2 = Query ("(" <> unQuery q1 <> " NOT " <> unQuery q2 <> ")")

  q ^=: n = Query (unQuery q <> "^=" <> Builder.show n)

  neg q = Query ("-" <> unQuery q)

  params ps q = Query ("{!" <> Builder.spaces (map compileParam ps) <> "}" <> unQuery q)
   where
    compileParam :: Param SolrQuery -> Builder
    compileParam (Param k v) =
      case k of
        SolrQueryDefaultField -> "df="   <> Text.encodeUtf8Builder v
        SolrQueryOp           -> "q.op=" <> Text.encodeUtf8Builder v

instance HasParamDefaultField SolrQuery where
  paramDefaultField = SolrQueryDefaultField

instance HasParamOp SolrQuery where
  paramOp = SolrQueryOp


-- | A Solr filter query. This is like 'SolrQuery', but with different local
-- parameters available. All functions polymorphic over 'SolrQuerySYM' will work
-- with both.
newtype SolrFilterQuery expr = FQuery { unFQuery :: SolrQuery expr }
  deriving Semigroup

-- | For debugging. Calls 'compileSolrFilterQuery'.
instance Show (SolrFilterQuery expr) where
  show = showb . compileSolrFilterQuery

instance SolrQuerySYM SolrExpr SolrFilterQuery where
  data ParamKey SolrFilterQuery a where
    SolrFilterQueryDefaultField :: ParamKey SolrFilterQuery Text
    SolrFilterQueryOp           :: ParamKey SolrFilterQuery Text
    SolrFilterQueryCache        :: ParamKey SolrFilterQuery Bool
    SolrFilterQueryCost         :: ParamKey SolrFilterQuery Int

  defaultField e = FQuery (defaultField e)

  f =: e = FQuery (f =: e)

  q1 &&: q2 = FQuery (unFQuery q1 &&: unFQuery q2)

  q1 ||: q2 = FQuery (unFQuery q1 ||: unFQuery q2)

  q1 -: q2 = FQuery (unFQuery q1 -: unFQuery q2)

  q ^=: n = FQuery (unFQuery q ^=: n)

  neg q = FQuery (neg (unFQuery q))

  -- Hm, for now it seems we have to duplicate this logic from SolrQuery.
  params ps q =
    FQuery (Query ("{!" <> Builder.spaces (map compileParam ps) <> "}" <> unQuery (unFQuery q)))
   where
    compileParam :: Param SolrFilterQuery -> Builder
    compileParam (Param k v) =
      case k of
        SolrFilterQueryDefaultField -> "df="    <> Text.encodeUtf8Builder v
        SolrFilterQueryOp           -> "q.op="  <> Text.encodeUtf8Builder v
        SolrFilterQueryCache        -> "cache=" <> if v then "true" else "false"
        SolrFilterQueryCost         -> "cost="  <> Builder.show v

instance HasParamDefaultField SolrFilterQuery where
  paramDefaultField = SolrFilterQueryDefaultField

instance HasParamOp SolrFilterQuery where
  paramOp = SolrFilterQueryOp

instance HasParamCache SolrFilterQuery where
  paramCache = SolrFilterQueryCache

instance HasParamCost SolrFilterQuery where
  paramCost = SolrFilterQueryCost


-- | Compile a 'SolrQuery' to a lazy 'ByteString'.
--
-- Note that the DSL admits many ways to create an invalid Solr query; that is,
-- if it compiles, it doesn't necessarily work. For example, multiple 'neg's on
-- a query, multiple 'params', etc.
--
-- >>> let ps = [paramDefaultField .= "body"]
-- >>> let q = "foo" =: phrase ["bar", "baz"] ~: 5 &&: defaultField (regex "wh?t") :: SolrQuery SolrExpr
-- >>> compileSolrQuery (params ps q)
-- "q={!df=body}(foo:\"bar baz\"~5 AND /wh?t/)"
compileSolrQuery :: SolrQuery expr -> LByteString.ByteString
compileSolrQuery = Builder.toLazyByteString . ("q=" <>) . unQuery

-- | Compile a 'SolrFilterQuery' to a lazy 'ByteString'.
compileSolrFilterQuery :: SolrFilterQuery expr -> LByteString.ByteString
compileSolrFilterQuery = Builder.toLazyByteString . ("fq=" <>) . unQuery . unFQuery


--------------------------------------------------------------------------------
-- Misc. helpers

-- Round-trip through UTF-8 encoded Text because ByteString.Char8 is BAD!
showb :: LByteString.ByteString -> String
showb = either show LText.unpack . LText.decodeUtf8'

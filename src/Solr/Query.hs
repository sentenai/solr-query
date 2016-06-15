{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Solr query construction and compilation. You may prefer to import
-- "Solr.Qualified.Query" instead, which does not export any operators.

module Solr.Query
  (
  -- * Query type
    SolrQuery
  , SolrFilterQuery
  -- * Query construction
  , defaultField
  , (=:)
  , (&&:)
  , (||:)
  , (-:)
  , (^=:)
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

import Solr.Class
import Solr.Param
import Solr.Type

import Data.Semigroup       (Semigroup(..))
import Data.String          (IsString(..))
import Data.Text            (Text)

import qualified Data.ByteString.Lazy    as LByteString
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Data.Text.Lazy          as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Text.Show.ByteString    as Text.Show.ByteString

-- Data.ByteString.Lazy.Builder was renamed to Data.ByteString.Builder in 0.10.2.0
#if MIN_VERSION_bytestring(0,10,2)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
#else
import Data.ByteString.Lazy.Builder (Builder)
import qualified Data.ByteString.Lazy.Builder as Builder
#endif

-- | A Solr expression.
newtype SolrExpr (t :: SolrType) = Expr { unExpr :: Builder }

instance IsString (SolrExpr 'TWord) where
  fromString s = word (Text.pack s)

instance SolrExprSYM SolrExpr where
  num n = Expr (bshow n)

  true = Expr "true"

  false = Expr "false"

  word s = Expr (Text.encodeUtf8Builder s)

  wild s = Expr (Text.encodeUtf8Builder s)

  regex s = Expr ("/" <> Text.encodeUtf8Builder s <> "/")

  phrase ss = Expr ("\"" <> spaces (map unExpr ss) <> "\"")

  e ~: n = Expr (unExpr e <> "~" <> bshow n)

  to b1 b2 = Expr (lhs b1 <> " TO " <> rhs b2)
   where
    lhs :: Boundary (SolrExpr a) -> Builder
    lhs (Inclusive e) = Builder.char8 '[' <> unExpr e
    lhs (Exclusive e) = Builder.char8 '{' <> unExpr e
    lhs Star          = Builder.lazyByteString "[*"

    rhs :: Boundary (SolrExpr a) -> Builder
    rhs (Inclusive e) = unExpr e <> Builder.char8 ']'
    rhs (Exclusive e) = unExpr e <> Builder.char8 '}'
    rhs Star          = Builder.lazyByteString "*]"

  e ^: n = Expr (unExpr e <> "^" <> bshow n)


-- | A Solr query.
newtype SolrQuery = Query { unQuery :: Builder }

-- | For debugging. Calls 'compileSolrQuery'.
instance Show SolrQuery where
  show = showb . compileSolrQuery

-- | Appending Solr queries simply puts a space between them. To Solr, this is
-- equivalent to combining them with \'OR\'. However, this behavior can be
-- adjusted on a per-query basis using 'paramOp'.
--
-- Due to limited precedence options, ('<>') will typically require parens
-- around its arguments.
instance Semigroup SolrQuery where
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

  q ^=: n = Query (unQuery q <> "^=" <> bshow n)

  neg q = Query ("-" <> unQuery q)

  params ps q = Query ("{!" <> spaces (map compileParam ps) <> "}" <> unQuery q)
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
newtype SolrFilterQuery = FQuery { unFQuery :: SolrQuery }
  deriving Semigroup

-- | For debugging. Calls 'compileSolrFilterQuery'.
instance Show SolrFilterQuery where
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
    FQuery (Query ("{!" <> spaces (map compileParam ps) <> "}" <> unQuery (unFQuery q)))
   where
    compileParam :: Param SolrFilterQuery -> Builder
    compileParam (Param k v) =
      case k of
        SolrFilterQueryDefaultField -> "df="    <> Text.encodeUtf8Builder v
        SolrFilterQueryOp           -> "q.op="  <> Text.encodeUtf8Builder v
        SolrFilterQueryCache        -> "cache=" <> if v then "true" else "false"
        SolrFilterQueryCost         -> "cost="  <> bshow v

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
-- >>> let q = "foo" =: phrase ["bar", "baz"] ~: 5 &&: defaultField (regex "wh?t")
-- >>> compileSolrQuery (params ps q)
-- "q={!df=body}(foo:\"bar baz\"~5 AND /wh?t/)"
compileSolrQuery :: SolrQuery -> LByteString.ByteString
compileSolrQuery = Builder.toLazyByteString . ("q=" <>) . unQuery

-- | Compile a 'SolrFilterQuery' to a lazy 'ByteString'.
compileSolrFilterQuery :: SolrFilterQuery -> LByteString.ByteString
compileSolrFilterQuery = Builder.toLazyByteString . ("fq=" <>) . unQuery . unFQuery


--------------------------------------------------------------------------------
-- Misc. helpers

bshow :: Text.Show.ByteString.Show a => a -> Builder
bshow = Builder.lazyByteString . Text.Show.ByteString.show

-- Round-trip through UTF-8 encoded Text because ByteString.Char8 is BAD!
showb :: LByteString.ByteString -> String
showb = either show LText.unpack . LText.decodeUtf8'

spaces :: [Builder] -> Builder
spaces [] = ""
spaces [w] = w
spaces (w:ws) = w <> " " <> spaces ws

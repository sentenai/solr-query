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
  -- $note-simplicity
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
  -- $note-simplicity
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

import Data.ByteString.Lazy.Char8 (ByteString)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid                (Monoid(..), (<>))
#else
import Data.Monoid                ((<>))
#endif
import Data.String                (IsString(..))
import Data.Text                  (Text)
import GHC.Exts                   (IsList(..))

import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString.Lazy.Char8 as BS

-- Data.ByteString.Lazy.Builder was renamed to Data.ByteString.Builder in 0.10.2.0
#if MIN_VERSION_bytestring(0,10,2)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
#else
import Data.ByteString.Lazy.Builder (Builder)
import qualified Data.ByteString.Lazy.Builder as BS
#endif

-- | A Solr expression.
newtype SolrExpr (t :: SolrType) = Expr { unExpr :: Builder }

instance IsString (SolrExpr 'TWord) where
  fromString s = word (T.pack s)

instance IsList (SolrExpr 'TPhrase) where
  type Item (SolrExpr 'TPhrase) = SolrExpr 'TWord

  fromList = phrase
  toList = map (Expr . BS.lazyByteString) . BS.words . BS.toLazyByteString . unExpr

instance SolrExprSYM SolrExpr where
  num n = Expr (bshow n)

  true = Expr "true"

  false = Expr "false"

  word s = Expr (T.encodeUtf8Builder s)

  wild s = Expr (T.encodeUtf8Builder s)

  regex s = Expr ("/" <> T.encodeUtf8Builder s <> "/")

  phrase ss = Expr ("\"" <> spaces (map unExpr ss) <> "\"")

  e ~: n = Expr (unExpr e <> "~" <> bshow n)

  to b1 b2 = Expr (lhs b1 <> " TO " <> rhs b2)
   where
    lhs :: Boundary (SolrExpr a) -> Builder
    lhs (Inclusive e) = BS.char8 '[' <> unExpr e
    lhs (Exclusive e) = BS.char8 '{' <> unExpr e
    lhs Star          = BS.lazyByteString "[*"

    rhs :: Boundary (SolrExpr a) -> Builder
    rhs (Inclusive e) = unExpr e <> BS.char8 ']'
    rhs (Exclusive e) = unExpr e <> BS.char8 '}'
    rhs Star          = BS.lazyByteString "*]"

  e ^: n = Expr (unExpr e <> "^" <> bshow n)


-- | A Solr query.
data SolrQuery = Query
  { qparams :: [Builder]
  , qbody   :: Builder
  }

-- | Appending Solr queries simply puts a space between them. To Solr, this is
-- equivalent to combining them with \'OR\'. However, this behavior can be
-- adjusted on a per-query basis using 'paramOp'.
--
-- Due to limited precedence options, ('<>') will typically require parens
-- around its arguments.
instance Monoid SolrQuery where
  mempty = Query mempty mempty
  mappend q1 q2 = Query (qparams q1 ++ qparams q2) (qbody q1 <> " " <> qbody q2)

instance SolrQuerySYM SolrExpr SolrQuery where
  data ParamKey SolrQuery a where
    SolrQueryDefaultField :: ParamKey SolrQuery Text
    SolrQueryOp           :: ParamKey SolrQuery Text

  defaultField e = Query mempty (unExpr e)

  f =: e = Query mempty (T.encodeUtf8Builder f <> ":" <> unExpr e)

  q1 &&: q2 =
    Query (qparams q1 ++ qparams q2)
          ("(" <> qbody q1 <> " AND " <> qbody q2 <> ")")

  q1 ||: q2 =
    Query (qparams q1 ++ qparams q2)
          ("(" <> qbody q1 <> " OR " <> qbody q2 <> ")")

  q1 -: q2 =
    Query (qparams q1 ++ qparams q2)
          ("(" <> qbody q1 <> " NOT " <> qbody q2 <> ")")

  q ^=: n = Query (qparams q) ("(" <> qbody q <> ")^=" <> bshow n)

  neg q = Query (qparams q) ("-" <> qbody q)

  params ps q = Query (map compileParam ps ++ qparams q) (qbody q)
   where
    compileParam :: Param SolrQuery -> Builder
    compileParam (Param k v) =
      case k of
        SolrQueryDefaultField -> "df=" <> T.encodeUtf8Builder v
        SolrQueryOp -> "q.op=" <> T.encodeUtf8Builder v

instance HasParamDefaultField SolrQuery where
  paramDefaultField = SolrQueryDefaultField

instance HasParamOp SolrQuery where
  paramOp = SolrQueryOp


-- | A Solr filter query. This is like 'SolrQuery', but with different local
-- parameters available. All functions polymorphic over 'SolrQuerySYM' will work
-- with both.
newtype SolrFilterQuery = FQuery { unFQuery :: SolrQuery }
  deriving Monoid

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
    FQuery (Query (map compileParam ps ++ qparams (unFQuery q))
                  (qbody (unFQuery q)))
   where
    compileParam :: Param SolrFilterQuery -> Builder
    compileParam (Param k v) =
      case k of
        SolrFilterQueryDefaultField -> "df=" <> T.encodeUtf8Builder v
        SolrFilterQueryOp -> "q.op=" <> T.encodeUtf8Builder v
        SolrFilterQueryCache -> "cache=" <> if v then "true" else "false"
        SolrFilterQueryCost -> "cost=" <> bshow v

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
-- Example:
--
-- @
-- λ let ps = ['paramDefaultField' '.=' "body"]
-- λ let q = "foo" =: 'phrase' ["bar", "baz"] '~:' 5 '&&:' 'defaultField' ('regex' "wh?at")
-- λ 'compileSolrQuery' ('params' ps q)
-- "{!df=body }(foo:\\"bar baz\\"~5 AND \/wh?t\/)"
-- @
compileSolrQuery :: SolrQuery -> ByteString
compileSolrQuery q =
  let
    body = BS.toLazyByteString (qbody q)
  in
    if null (qparams q)
      then body
      else "{!" <> BS.toLazyByteString (spaces (qparams q)) <> "}" <> body

-- | Compile a 'SolrFilterQuery' to a lazy 'ByteString'.
compileSolrFilterQuery :: SolrFilterQuery -> ByteString
compileSolrFilterQuery = compileSolrQuery . unFQuery


bshow :: Show a => a -> Builder
bshow = BS.lazyByteString . BS.pack . show

spaces :: [Builder] -> Builder
spaces [] = ""
spaces [w] = w
spaces (w:ws) = w <> " " <> spaces ws


-- $note-simplicity
-- For simplicity, the type signatures in the examples below monomorphise the
-- functions to use 'SolrQuery' (and therefore 'SolrExpr', due to the
-- functional dependency).

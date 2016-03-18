{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Solr query construction and compilation. You may prefer to import
-- "Solr.Query.Qualified" instead, which does not contain any operators.

module Solr.Query
  (
  -- * Query type
    SolrQuery
  -- * Query construction
  -- $note-simplicity
  , (=:)
  , (&&:)
  , (||:)
  , (-:)
  , (^=:)
  -- * Expression type
  , SolrExpr
  -- * Expression construction
  -- $note-simplicity
  , int
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
  -- * Query compilation
  , compileSolrQuery
  ) where

import Solr.Class
import Solr.Type

import Data.ByteString.Builder    (Builder)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Monoid
import Data.String                (IsString(..))
import GHC.Exts                   (IsList(..))

import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString.Builder    as BS
import qualified Data.ByteString.Lazy.Char8 as BS

-- | A Solr query.
newtype SolrQuery = Query { unQuery :: Builder }


-- | A Solr expression.
newtype SolrExpr (t :: SolrType) = Expr { unExpr :: Builder }

-- | This instance is only provided for convenient numeric literals. /ALL/ 'Num'
-- functions besides 'fromInteger' are not implemented and will cause a runtime
-- crash.
instance Num (SolrExpr 'TInt) where
  (+) = error "SolrExpr.Num.(+): not implemented"
  (*) = error "SolrExpr.Num.(*): not implemented"
  abs = error "SolrExpr.Num.abs: not implemented"
  signum = error "SolrExpr.Num.signum: not implemented"
  negate = error "SolrExpr.Num.negate: not implemented"

  fromInteger i = int (fromInteger i)

instance IsString (SolrExpr 'TWord) where
  fromString s = word (T.pack s)

instance IsList (SolrExpr 'TPhrase) where
  type Item (SolrExpr 'TPhrase) = SolrExpr 'TWord

  fromList = phrase
  toList = map (Expr . BS.lazyByteString) . BS.words . BS.toLazyByteString . unExpr


instance Solr SolrExpr SolrQuery where
  int n = Expr (bshow n)

  true = Expr "true"

  false = Expr "false"

  word s = Expr (T.encodeUtf8Builder s)

  wild s = Expr (T.encodeUtf8Builder s)

  regex s = Expr ("/" <> T.encodeUtf8Builder s <> "/")

  phrase ss = Expr ("\"" <> spaces ss <> "\"")
   where
    spaces [] = ""
    spaces [w] = unExpr w
    spaces (w:ws) = unExpr w <> " " <> spaces ws

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

  f =: e = Query (T.encodeUtf8Builder f <> ":" <> unExpr e)

  q1 &&: q2 = Query ("(" <> unQuery q1 <> " AND " <> unQuery q2 <> ")")

  q1 ||: q2 = Query ("(" <> unQuery q1 <> " OR " <> unQuery q2 <> ")")

  q1 -: q2 = Query ("(" <> unQuery q1 <> " NOT " <> unQuery q2 <> ")")

  q ^=: n = Query ("((" <> unQuery q <> ")^=" <> bshow n <> ")")

bshow :: Show a => a -> Builder
bshow = BS.lazyByteString . BS.pack . show


-- | Compile a 'SolrQuery' to a lazy 'ByteString'. Because the underlying
-- expressions are correct by consutruction, this function is total.
compileSolrQuery :: SolrQuery -> ByteString
compileSolrQuery = BS.toLazyByteString . unQuery


-- $note-simplicity
-- For simplicity, the type signatures in the examples below monomorphise the
-- functions to use 'SolrQuery' (and therefore 'SolrExpr', due to the
-- functional dependency).

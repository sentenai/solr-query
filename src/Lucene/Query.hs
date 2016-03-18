{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Lucene query construction and compilation. You may prefer to import
-- "Lucene.Query.Qualified" instead, which does not contain any operators.

module Lucene.Query
  (
  -- * Query type
    LuceneQuery
  -- * Query construction
  -- $note-simplicity
  , (=:)
  , (&&:)
  , (||:)
  , (-:)
  , (^=:)
  -- * Expression type
  , LuceneExpr
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
  , compileLuceneQuery
  ) where

import Lucene.Class
import Lucene.Type

import Data.ByteString.Builder    (Builder)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Monoid
import Data.String                (IsString(..))
import GHC.Exts                   (IsList(..))

import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString.Builder    as BS
import qualified Data.ByteString.Lazy.Char8 as BS

-- | A Lucene query.
newtype LuceneQuery = Query { unQuery :: Builder }


-- | A Lucene expression.
newtype LuceneExpr (t :: LuceneType) = Expr { unExpr :: Builder }

-- | This instance is only provided for convenient numeric literals. /ALL/ 'Num'
-- functions besides 'fromInteger' are not implemented and will cause a runtime
-- crash.
instance Num (LuceneExpr 'TInt) where
  (+) = error "LuceneExpr.Num.(+): not implemented"
  (*) = error "LuceneExpr.Num.(*): not implemented"
  abs = error "LuceneExpr.Num.abs: not implemented"
  signum = error "LuceneExpr.Num.signum: not implemented"
  negate = error "LuceneExpr.Num.negate: not implemented"

  fromInteger i = int (fromInteger i)

instance IsString (LuceneExpr 'TWord) where
  fromString s = word (T.pack s)

instance IsList (LuceneExpr 'TPhrase) where
  type Item (LuceneExpr 'TPhrase) = LuceneExpr 'TWord

  fromList = phrase
  toList = map (Expr . BS.lazyByteString) . BS.words . BS.toLazyByteString . unExpr


instance Lucene LuceneExpr LuceneQuery where
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
    lhs :: Boundary (LuceneExpr a) -> Builder
    lhs (Inclusive e) = BS.char8 '[' <> unExpr e
    lhs (Exclusive e) = BS.char8 '{' <> unExpr e
    lhs Star          = BS.lazyByteString "[*"

    rhs :: Boundary (LuceneExpr a) -> Builder
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


-- | Compile a 'LuceneQuery' to a lazy 'ByteString'. Because the underlying
-- expressions are correct by consutruction, this function is total.
compileLuceneQuery :: LuceneQuery -> ByteString
compileLuceneQuery = BS.toLazyByteString . unQuery


-- $note-simplicity
-- For simplicity, the type signatures in the examples below monomorphise the
-- functions to use 'LuceneQuery' (and therefore 'LuceneExpr', due to the
-- functional dependency).

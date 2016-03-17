{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Lucene.Query.Typed
  ( LuceneQuery
  , (=:), (&&:), (||:), (-:)
  , compileLuceneQuery
  ) where

import Lucene.Expr.Typed

import Data.ByteString.Builder    (Builder)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Monoid
import Data.Text                  (Text)

import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Builder as BS


data LuceneQuery
  = forall a. QField Text (LuceneExpr a)
  | QAnd LuceneQuery LuceneQuery
  | QOr LuceneQuery LuceneQuery
  | QNot LuceneQuery LuceneQuery


(=:) :: Text -> LuceneExpr a -> LuceneQuery
(=:) = QField

(&&:) :: LuceneQuery -> LuceneQuery -> LuceneQuery
(&&:) = QAnd

(||:) :: LuceneQuery -> LuceneQuery -> LuceneQuery
(||:) = QOr

(-:) :: LuceneQuery -> LuceneQuery -> LuceneQuery
(-:) = QNot


compileLuceneQuery :: LuceneQuery -> ByteString
compileLuceneQuery = BS.toLazyByteString . go
 where
  go :: LuceneQuery -> Builder
  go (QField f e) = T.encodeUtf8Builder f <> ":" <> compileLuceneExpr e
  go (QAnd q1 q2) = "(" <> go q1 <> " AND " <> go q2 <> ")"
  go (QOr q1 q2) = "(" <> go q1 <> " OR " <> go q2 <> ")"
  go (QNot q1 q2) = "(" <> go q1 <> " NOT " <> go q2 <> ")"

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Lucene.Query.Typed
  (
  -- * Query type
    LuceneQuery
  -- * Query construction
  , (=:), (&&:), (||:), (-:)
  -- * Query compilation
  , compileLuceneQuery
  ) where

import Lucene.Expr.Typed

import Data.ByteString.Builder    (Builder)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Monoid
import Data.Text                  (Text)

import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Builder as BS


-- | A Lucene query.
data LuceneQuery
  = forall a. QField Text (LuceneExpr a)
  | QAnd LuceneQuery LuceneQuery
  | QOr LuceneQuery LuceneQuery
  | QNot LuceneQuery LuceneQuery


-- | A field query.
--
-- Example:
--
-- @
-- -- foo:bar
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'word' "bar"
-- @
(=:) :: Text -> LuceneExpr a -> LuceneQuery
(=:) = QField
infix 4 =:

-- | An @AND@ query.
--
-- Example:
--
-- @
-- -- foo:bar AND baz:qux
-- query :: 'LuceneQuery'
-- query =
--       "foo" '=:' 'word' "bar"
--   '&&:' "baz" '=:' 'word' "qux"
-- @
(&&:) :: LuceneQuery -> LuceneQuery -> LuceneQuery
(&&:) = QAnd
infixr 3 &&:

-- | An @OR@ query.
--
-- Example:
--
-- @
-- -- foo:bar OR baz:qux
-- query :: 'LuceneQuery'
-- query =
--       "foo" '=:' 'word' "bar"
--   '||:' "baz" '=:' 'word' "qux"
-- @
(||:) :: LuceneQuery -> LuceneQuery -> LuceneQuery
(||:) = QOr
infixr 2 ||:

-- | A @NOT@ query.
--
-- Example:
--
-- @
-- -- foo:bar NOT baz:qux
-- query :: 'LuceneQuery'
-- query =
--      "foo" '=:' 'word' "bar"
--   '-:' "baz" '=:' 'word' "qux"
-- @
(-:) :: LuceneQuery -> LuceneQuery -> LuceneQuery
(-:) = QNot
infixr 1 -:


-- | Compile a Lucene query to a 'ByteString'. Because the underlying Lucene
-- expressions are correct by construction, this function is total.
compileLuceneQuery :: LuceneQuery -> ByteString
compileLuceneQuery = BS.toLazyByteString . go
 where
  go :: LuceneQuery -> Builder
  go (QField f e) = T.encodeUtf8Builder f <> ":" <> compileLuceneExpr e
  go (QAnd q1 q2) = "(" <> go q1 <> " AND " <> go q2 <> ")"
  go (QOr q1 q2) = "(" <> go q1 <> " OR " <> go q2 <> ")"
  go (QNot q1 q2) = "(" <> go q1 <> " NOT " <> go q2 <> ")"

module Solr.Query.Lucene.Internal.Internal where

import Solr.Prelude

import Builder
import Solr.Query.Internal.Internal
import Solr.Query.Lucene.Expr
import Solr.Query.Lucene.Expr.Type

newtype LuceneQuery
  = Q { unQ :: Builder }

instance Query LuceneQuery where
  data LocalParams LuceneQuery = LuceneParams
    { paramDf :: Maybe Text
    , paramQOp :: Maybe QOp
    }

  compileLocalParams :: LocalParams LuceneQuery -> [(Builder, Builder)]
  compileLocalParams (LuceneParams{paramDf, paramQOp}) = catMaybes
    [ compileDf <$> paramDf
    , compileQOp <$> paramQOp
    ]
   where
    compileDf :: Text -> (Builder, Builder)
    compileDf v = ("df", thaw' v)

    compileQOp :: QOp -> (Builder, Builder)
    compileQOp QOpAnd = ("q.op", "AND")
    compileQOp QOpOr  = ("q.op", "OR")

data QOp
  = QOpAnd
  | QOpOr

instance Default (LocalParams LuceneQuery) where
  def = LuceneParams Nothing Nothing

-- | The @\'df\'@ local parameter.
df :: Text -> LocalParams LuceneQuery -> LocalParams LuceneQuery
df x s = s { paramDf = Just x }

-- | The @\'op=AND\'@ local parameter.
opAnd :: LocalParams LuceneQuery -> LocalParams LuceneQuery
opAnd s = s { paramQOp = Just QOpAnd }

-- | The @\'op=OR\'@ local parameter.
opOr :: LocalParams LuceneQuery -> LocalParams LuceneQuery
opOr s = s { paramQOp = Just QOpOr }

-- | Negate a 'LuceneQuery'.
neg :: LuceneQuery -> LuceneQuery
neg (Q q) = coerce (parens ("*:* NOT " <> q))

-- | A default field query.
defaultField :: LuceneExpr ty -> LuceneQuery
defaultField (E q) = Q q

-- | A field query.
(=:) :: Text -> LuceneExpr ty -> LuceneQuery
f =: E e = Q (thaw' f <> char ':' <> e)
infix 7 =:

-- | Named version of ('=:').
field :: Text -> LuceneExpr ty -> LuceneQuery
field = (=:)

-- | The @\'^=\'@ constant score operator.
--
-- This is given right-fixity to reject queries like @q ^= 1 ^= 2@.
(^=:) :: LuceneQuery -> Float -> LuceneQuery
Q q ^=: n = Q (q <> "^=" <> bshow n)
infixr 6 ^=:

-- | Named version of ('^=:').
score :: LuceneQuery -> Float -> LuceneQuery
score = (^=:)
infixr 6 `score`

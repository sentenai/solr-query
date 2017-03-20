module Solr.Query.Lucene
  ( -- * Lucene query
    LuceneQuery
  , defaultField
  , (=:)
  , field
  , (^=:)
  , score
    -- ** Lucene expression
  , LuceneExpr
    -- *** @int@ expression
  , int
    -- *** @float@ expression
  , float
    -- *** @bool@ expressions
  , true
  , false
    -- *** @word@ expression
  , word
    -- *** @wild@ expression
  , wild
    -- *** @regex@ expression
  , regex
    -- *** @phrase@ expression
  , phrase
    -- *** @datetime@ expression
  , datetime
  , DateTime
  , Year
  , Month
  , Day
  , Hour
  , Minute
  , Second
  , Millisecond
    -- *** @fuzz@ expression
  , (~:)
  , fuzz
  , fuzzy
    -- *** @boost@ expression
  , (^:)
  , boost
    -- *** @range@ expressions
  , to
  , gt
  , gte
  , lt
  , lte
  , Boundary
  , incl
  , excl
  , star
    -- *** @spatial predicate@ expressions
  , intersects
  , isWithin
  , Shape
  , polygon
    -- *** Lucene expression types
  , LuceneExprTy(..)
  , Fuzzable
  , Boostable
  , Rangeable
    -- ** Local parameters
  , df
  , opAnd
  , opOr
    -- * Re-exports
  , UTCTime
  ) where

import Solr.Prelude

import Builder
import Solr.Query.Internal
import Solr.Query.Lucene.Expr
import Solr.Query.Lucene.Expr.Type

newtype LuceneQuery
  = Q { unQ :: Builder }

instance Monoid LuceneQuery where
  mempty :: LuceneQuery
  mempty = defaultMempty

  mappend :: LuceneQuery -> LuceneQuery -> LuceneQuery
  mappend = defaultMappend

instance Query LuceneQuery where
  data LocalParams LuceneQuery = LuceneParams
    { _df :: Maybe Text
    , _qop :: Maybe QOp
    }

  compileLocalParams :: LocalParams LuceneQuery -> [Builder]
  compileLocalParams (LuceneParams{_df, _qop}) = catMaybes
    [ compileDf <$> _df
    , compileQOp <$> _qop
    ]
   where
    compileDf :: Text -> Builder
    compileDf v = "df=" <> thaw' v

    compileQOp :: QOp -> Builder
    compileQOp QOpAnd = "q.op=AND"
    compileQOp QOpOr  = "q.op=OR"

data QOp
  = QOpAnd
  | QOpOr

instance Default (LocalParams LuceneQuery) where
  def = LuceneParams Nothing Nothing

-- | The @\'df\'@ local parameter.
df :: Text -> LocalParams LuceneQuery -> LocalParams LuceneQuery
df x s = s { _df = Just x }

-- | The @\'op=AND\'@ local parameter.
opAnd :: LocalParams LuceneQuery -> LocalParams LuceneQuery
opAnd s = s { _qop = Just QOpAnd }

-- | The @\'op=OR\'@ local parameter.
opOr :: LocalParams LuceneQuery -> LocalParams LuceneQuery
opOr s = s { _qop = Just QOpOr }

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

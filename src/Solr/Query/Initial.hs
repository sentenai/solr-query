{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE PatternSynonyms #-}

-- | An initial encoding of a Solr query. This is an alternative interpretation
-- of the Solr language that is more amenable to parsing from arbitrary user
-- input and applying query transformations.

module Solr.Query.Initial
  ( -- * Initially-encoded Query
    Query(..)
  , typeCheck
  , factor
  , reinterpret
    -- * Re-exports
  , module Solr.Param
  , module Solr.Query.Class
  ) where

import Solr.Param
import Solr.Param.Internal
import Solr.Query.Class
import Solr.Type               (SolrType)

import qualified Solr.Expr.Initial.Untyped as UExpr
import qualified Solr.Expr.Initial.Typed   as Expr

import Control.Applicative           (pure)
import Data.Coerce                   (coerce)
import Data.Function                 (fix)
import Data.Generics.Uniplate.Direct (Uniplate(..), (|-), (|*), plate, transform)
import Data.Generics.Str             (Str)
import Data.Semigroup                (Semigroup(..))
import Data.Text (Text)

#if WITH_CONSTRAINTS
import Data.Constraint        ((:-), (\\))
import Data.Constraint.Forall
import GHC.Show               (showSpace)
#endif

-- | An initial encoding of a Solr query. To get a 'Show' instance, compile with
-- @-fwith-constraints@.
data Query (expr :: SolrType -> *)
  = forall a. QDefaultField (expr a)
  | forall a. QField Text (expr a)
  | QAnd (Query expr) (Query expr)
  | QOr (Query expr) (Query expr)
  | QNot (Query expr) (Query expr)
  | QScore (Query expr) Float
  | QAppend (Query expr) (Query expr)

instance Eq (Query UExpr.Expr) where
  QDefaultField a   == QDefaultField c   = coerce a == c
  QField        a b == QField        c d =        a == c && coerce b == d
  QAnd          a b == QAnd          c d =        a == c &&        b == d
  QOr           a b == QOr           c d =        a == c &&        b == d
  QNot          a b == QNot          c d =        a == c &&        b == d
  QScore        a b == QScore        c d =        a == c &&        b == d
  QAppend       a b == QAppend       c d =        a == c &&        b == d
  _                 == _                 = False

#if WITH_CONSTRAINTS
-- This might be the ugliest Show instance I've ever written
instance ForallF Show expr => Show (Query expr) where
  showsPrec n = \case
    QDefaultField (e :: expr a) ->
      showParen (n >= 11) (showString "QDefaultField " . showsPrec 11 e)
        \\ (instF :: ForallF Show expr :- Show (expr a))
    QField s (e :: expr a) ->
      showParen (n >= 11) (showString "QField " . showsPrec 11 s . showSpace . showsPrec 11 e)
        \\ (instF :: ForallF Show expr :- Show (expr a))

    QAnd q1 q2    -> showParen (n >= 11) (showString "QAnd "    . showsPrec 11 q1 . showSpace . showsPrec 11 q2)
    QOr q1 q2     -> showParen (n >= 11) (showString "QOr "     . showsPrec 11 q1 . showSpace . showsPrec 11 q2)
    QNot q1 q2    -> showParen (n >= 11) (showString "QNot "    . showsPrec 11 q1 . showSpace . showsPrec 11 q2)
    QScore q m    -> showParen (n >= 11) (showString "QScore "  . showsPrec 11 q  . showSpace . showsPrec 11 m)
    QAppend q1 q2 -> showParen (n >= 11) (showString "QAppend " . showsPrec 11 q1 . showSpace . showsPrec 11 q2)
#endif

-- | Technically not a law-abiding 'Semigroup' instance, as you can observe the
-- associativity of '<>'. It's up to interpreters to use this instance
-- correctly.
instance Semigroup (Query expr) where
  (<>) = QAppend

instance Uniplate (Query expr) where
  uniplate :: Query expr -> (Str (Query expr), Str (Query expr) -> Query expr)
  uniplate = \case
    QDefaultField e -> plate QDefaultField |- e
    QField n e      -> plate QField |- n |- e
    QAnd q1 q2      -> plate QAnd |* q1 |* q2
    QOr q1 q2       -> plate QOr |* q1 |* q2
    QNot q1 q2      -> plate QNot |* q1 |* q2
    QScore q n      -> plate QScore |* q |- n
    QAppend q1 q2   -> plate QAppend |* q1 |* q2

instance ExprSYM expr => QuerySYM expr Query where
  defaultField = QDefaultField
  (=:)         = QField
  (&&:)        = QAnd
  (||:)        = QOr
  (-:)         = QNot
  (^=:)        = QScore

instance HasParamDefaultField Query
instance HasParamOp           Query
instance HasParamRows         Query
instance HasParamStart        Query

pattern QNeg :: Query Expr.Expr -> Query Expr.Expr
pattern QNeg q = QNot (QField "*" (Expr.ETo Star Star)) q

-- | Type check an untyped Solr query. Note the untyped 'UExpr.Expr' input is
-- not the same as the typed 'Expr.Expr' output.
typeCheck :: Query UExpr.Expr -> Maybe (Query Expr.Expr)
typeCheck u0 =
  case u0 of
    QDefaultField u -> Expr.typeCheck u (fmap QDefaultField)

    QField s u -> Expr.typeCheck u (fmap (QField s))

    QAnd    u1 u2 -> binop QAnd    u1 u2
    QOr     u1 u2 -> binop QOr     u1 u2
    QNot    u1 u2 -> binop QNot    u1 u2
    QAppend u1 u2 -> binop QAppend u1 u2

    QScore u n -> do
      q <- typeCheck u
      pure (QScore q n)

 where
  binop
    :: (Query Expr.Expr -> Query Expr.Expr -> Query Expr.Expr)
    -> Query UExpr.Expr
    -> Query UExpr.Expr
    -> Maybe (Query Expr.Expr)
  binop con u1 u2 = do
    q1 <- typeCheck u1
    q2 <- typeCheck u2
    pure (con q1 q2)

-- | Factor a type-safe 'Query' 'Expr.Expr' into a canonical form (e.g. perform
-- double-negation elimination). Check the source code for all transformations
-- performed.
factor :: Query Expr.Expr -> Query Expr.Expr
factor =
    transform rightAssocAppend
  . transform doubleNegationElim
  . transform elimInnerScores
 where
  -- Eliminate all scores inside of a scored query (essentially, the outermost
  -- score takes precedence).
  elimInnerScores :: Query expr -> Query expr
  elimInnerScores = \case
    QScore q n -> QScore (unscore q) n
    q          -> q
   where
    -- Because the outer transformation is bottom-up, we can stop at the first
    -- QScore we find (top-down).
    unscore :: Query expr -> Query expr
    unscore = \case
      QAnd q1 q2    -> QAnd (unscore q1) (unscore q2)
      QOr q1 q2     -> QOr (unscore q1) (unscore q2)
      QNot q1 q2    -> QNot (unscore q1) (unscore q2)
      QScore q _    -> q -- note, not 'unscore q'
      QAppend q1 q2 -> QAppend (unscore q1) (unscore q2)
      q             -> q

  -- Rewrite -(-q) as q
  doubleNegationElim :: Query Expr.Expr -> Query Expr.Expr
  doubleNegationElim = \case
    QNeg (QNeg q) -> q
    q -> q

  -- Rewrite ((q1 <> q2) <> q3) as (q1 <> (q2 <> q3))
  rightAssocAppend :: Query expr -> Query expr
  rightAssocAppend = \case
    QAppend (QAppend q1 q2) q3 -> QAppend q1 (QAppend q2 q3)
    q -> q


-- | Reinterpret an initially-encoded 'Query' to some other interpretation.
--
-- This may be useful for reinterpreting a 'Query' as a lazy
-- 'Data.Text.Lazy.Text' after it's been type checked and factored with the
-- machinery in this module.
reinterpret
  :: forall expr query.
     ( QuerySYM expr query
     , Semigroup (query expr)
     )
  => Query Expr.Expr
  -> query expr
reinterpret = fix $ \r -> \case
  QDefaultField e -> defaultField (Expr.reinterpret e)
  QField s e      -> field s (Expr.reinterpret e)
  QAnd q1 q2      -> r q1 &&: r q2
  QOr q1 q2       -> r q1 ||: r q2
  QNot q1 q2      -> r q1 -: r q2
  QScore q n      -> r q ^=: n
  QAppend q1 q2   -> r q1 <> r q2

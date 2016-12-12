{-# language CPP #-}

#if WITH_CONSTRAINTS
{-# language UndecidableInstances #-}
#endif

#if MIN_VERSION_base(4,9,0)
{-# options_ghc -fno-warn-redundant-constraints #-}
#endif

module Solr.Query.Impl.Initial where

import Solr.Expr.Class
import Solr.Expr.Impl.Initial.Typed
import Solr.Expr.Impl.Initial.Untyped
import Solr.Expr.Type
import Solr.Query.Class
import Solr.Prelude

import Data.Generics.Uniplate.Direct
  (Uniplate(..), (|-), (|*), plate, transform)
import Data.Generics.Str (Str)

#if WITH_CONSTRAINTS
import Data.Constraint ((:-), (\\))
import Data.Constraint.Forall
import GHC.Show (showSpace)
#endif

-- | An initial encoding of a Solr query. To get a 'Show' instance, compile with
-- @-fwith-constraints@.
data Q (expr :: ExprTy -> *)
  = forall a. QDefaultField (expr a)
  | forall a. QField Text (expr a)
  | QAnd (Q expr) (Q expr)
  | QOr (Q expr) (Q expr)
  | QNot (Q expr) (Q expr)
  | QScore (Q expr) Float
  | QAppend (Q expr) (Q expr)

instance Eq (Q UExpr) where
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
instance ForallF Show expr => Show (Q expr) where
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
instance Semigroup (Q expr) where
  (<>) = QAppend

instance Uniplate (Q expr) where
  uniplate :: Q expr -> (Str (Q expr), Str (Q expr) -> Q expr)
  uniplate = \case
    QDefaultField e -> plate QDefaultField |- e
    QField n e      -> plate QField |- n |- e
    QAnd q1 q2      -> plate QAnd |* q1 |* q2
    QOr q1 q2       -> plate QOr |* q1 |* q2
    QNot q1 q2      -> plate QNot |* q1 |* q2
    QScore q n      -> plate QScore |* q |- n
    QAppend q1 q2   -> plate QAppend |* q1 |* q2

instance ExprSYM expr => QuerySYM expr Q where
  defaultField = QDefaultField
  (=:)         = QField
  (&&:)        = QAnd
  (||:)        = QOr
  (-:)         = QNot
  (^=:)        = QScore

#if MIN_VERSION_base(4,8,0)
pattern QNeg :: Q Expr -> Q Expr
#endif
pattern QNeg q = QNot (QField "*" (ETo Star Star)) q

-- | Type check an untyped Solr query.
typeCheck :: Q UExpr -> Maybe (Q Expr)
typeCheck u0 =
  case u0 of
    QDefaultField u -> typeCheckE u (fmap QDefaultField)

    QField s u -> typeCheckE u (fmap (QField s))

    QAnd    u1 u2 -> binop QAnd    u1 u2
    QOr     u1 u2 -> binop QOr     u1 u2
    QNot    u1 u2 -> binop QNot    u1 u2
    QAppend u1 u2 -> binop QAppend u1 u2

    QScore u n -> do
      q <- typeCheck u
      pure (QScore q n)

 where
  binop :: (Q Expr -> Q Expr -> Q Expr) -> Q UExpr -> Q UExpr -> Maybe (Q Expr)
  binop con u1 u2 = do
    q1 <- typeCheck u1
    q2 <- typeCheck u2
    pure (con q1 q2)

-- | Factor a query into a canonical form (e.g. perform double-negation
-- elimination). Check the source code for all transformations performed.
factor :: Q Expr -> Q Expr
factor =
    transform rightAssocAppend
  . transform doubleNegationElim
  . transform elimInnerScores
 where
  -- Eliminate all scores inside of a scored query (essentially, the outermost
  -- score takes precedence).
  elimInnerScores :: Q expr -> Q expr
  elimInnerScores = \case
    QScore q n -> QScore (unscore q) n
    q          -> q
   where
    -- Because the outer transformation is bottom-up, we can stop at the first
    -- QScore we find (top-down).
    unscore :: Q expr -> Q expr
    unscore = \case
      QAnd q1 q2    -> QAnd (unscore q1) (unscore q2)
      QOr q1 q2     -> QOr (unscore q1) (unscore q2)
      QNot q1 q2    -> QNot (unscore q1) (unscore q2)
      QScore q _    -> q -- note, not 'unscore q'
      QAppend q1 q2 -> QAppend (unscore q1) (unscore q2)
      q             -> q

  -- Rewrite -(-q) as q
  doubleNegationElim :: Q Expr -> Q Expr
  doubleNegationElim = \case
    QNeg (QNeg q) -> q
    q -> q

  -- Rewrite ((q1 <> q2) <> q3) as (q1 <> (q2 <> q3))
  rightAssocAppend :: Q expr -> Q expr
  rightAssocAppend = \case
    QAppend (QAppend q1 q2) q3 -> QAppend q1 (QAppend q2 q3)
    q -> q

-- | Reinterpret an initially-encoded 'Query' to some other interpretation.
--
-- This may be useful for reinterpreting a 'Query' as a lazy
-- 'Data.Text.Lazy.Text' after it's been type checked and factored with the
-- machinery in this module.
reinterpret :: Q Expr -> Query
reinterpret = fix $ \r -> \case
  QDefaultField e -> defaultField (reinterpretE e)
  QField s e      -> field s (reinterpretE e)
  QAnd q1 q2      -> r q1 &&: r q2
  QOr q1 q2       -> r q1 ||: r q2
  QNot q1 q2      -> r q1 -: r q2
  QScore q n      -> r q ^=: n
  QAppend q1 q2   -> r q1 <> r q2

{-# LANGUAGE UndecidableInstances #-}

-- | An initial encoding of a Solr query. This is an alternative interpretation
-- of the Solr language that is more amenable to parsing from arbitrary user
-- input and applying query transformations.

module Solr.Query.Initial
  ( -- * Query type
    SolrQuery(..)
    -- * Type checking
  , typeCheckSolrQuery
    -- * Factorization
  , factorSolrQuery
    -- * Reinterpretation
  , reinterpretSolrQuery
    -- * Re-exports
  , module Solr.Internal.Class.Query
  , module Solr.Query.Param
  ) where

import Solr.Expr.Initial.Typed   (typeCheckSolrExpr)
import Solr.Internal.Class.Query
import Solr.Query.Param
import Solr.Query.Param.Internal
import Solr.Type                 (SolrType)

import qualified Solr.Expr.Initial.Untyped as Untyped
import qualified Solr.Expr.Initial.Typed   as Typed

import Data.Coerce                   (coerce)
import Data.Constraint               ((:-), (\\))
import Data.Constraint.Forall
import Data.Function                 (fix)
import Data.Generics.Uniplate.Direct (Uniplate(..), (|-), (|*), plate, transform)
import Data.Generics.Str             (Str)
import Data.Semigroup                (Semigroup(..))
import Data.Text (Text)
import GHC.Show                      (showSpace)


-- | An initial encoding of a Solr query.
data SolrQuery (expr :: SolrType -> *)
  = forall a. QDefaultField (expr a)
  | forall a. QField Text (expr a)
  | QAnd (SolrQuery expr) (SolrQuery expr)
  | QOr (SolrQuery expr) (SolrQuery expr)
  | QNot (SolrQuery expr) (SolrQuery expr)
  | QScore (SolrQuery expr) Float
  | QNeg (SolrQuery expr)
  | QAppend (SolrQuery expr) (SolrQuery expr)

instance Eq (SolrQuery Untyped.SolrExpr) where
  QDefaultField a   == QDefaultField c   = coerce a == c
  QField        a b == QField        c d =        a == c && coerce b == d
  QAnd          a b == QAnd          c d =        a == c &&        b == d
  QOr           a b == QOr           c d =        a == c &&        b == d
  QNot          a b == QNot          c d =        a == c &&        b == d
  QScore        a b == QScore        c d =        a == c &&        b == d
  QNeg          a   == QNeg          c   =        a == c
  QAppend       a b == QAppend       c d =        a == c &&        b == d
  _                 == _                 = False

-- This might be the ugliest Show instance I've ever written
instance ForallF Show expr => Show (SolrQuery expr) where
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
    QNeg q        -> showParen (n >= 11) (showString "QNeg "    . showsPrec 11 q)
    QAppend q1 q2 -> showParen (n >= 11) (showString "QAppend " . showsPrec 11 q1 . showSpace . showsPrec 11 q2)

-- | Technically not a law-abiding 'Semigroup' instance, as you can observe the
-- associativity of '<>'. It's up to interpreters to use this instance
-- correctly.
instance Semigroup (SolrQuery expr) where
  (<>) = QAppend

instance Uniplate (SolrQuery expr) where
  uniplate :: SolrQuery expr -> (Str (SolrQuery expr), Str (SolrQuery expr) -> SolrQuery expr)
  uniplate = \case
    QDefaultField e -> plate QDefaultField |- e
    QField n e      -> plate QField |- n |- e
    QAnd q1 q2      -> plate QAnd |* q1 |* q2
    QOr q1 q2       -> plate QOr |* q1 |* q2
    QNot q1 q2      -> plate QNot |* q1 |* q2
    QScore q n      -> plate QScore |* q |- n
    QNeg q          -> plate QNeg |* q
    QAppend q1 q2   -> plate QAppend |* q1 |* q2

instance SolrExprSYM expr => SolrQuerySYM expr SolrQuery where
  defaultField = QDefaultField
  (=:)         = QField
  (&&:)        = QAnd
  (||:)        = QOr
  (-:)         = QNot
  (^=:)        = QScore
  neg          = QNeg

instance HasParamDefaultField SolrQuery
instance HasParamOp           SolrQuery
instance HasParamRows        SolrQuery
instance HasParamStart        SolrQuery

-- | Type check an untyped Solr query. Note the untyped 'Untyped.SolrExpr' on
-- the way in is not the same as the typed 'Typed.SolrExpr' on the way out.
typeCheckSolrQuery :: SolrQuery Untyped.SolrExpr -> Maybe (SolrQuery Typed.SolrExpr)
typeCheckSolrQuery u0 =
  case u0 of
    QDefaultField u -> typeCheckSolrExpr u (fmap QDefaultField)

    QField s u -> typeCheckSolrExpr u (fmap (QField s))

    QAnd    u1 u2 -> binop QAnd    u1 u2
    QOr     u1 u2 -> binop QOr     u1 u2
    QNot    u1 u2 -> binop QNot    u1 u2
    QAppend u1 u2 -> binop QAppend u1 u2

    QScore u n -> do
      q <- typeCheckSolrQuery u
      pure (QScore q n)

    QNeg u -> do
      q <- typeCheckSolrQuery u
      pure (QNeg q)

 where
  binop
    :: (SolrQuery Typed.SolrExpr -> SolrQuery Typed.SolrExpr -> SolrQuery Typed.SolrExpr)
    -> SolrQuery Untyped.SolrExpr
    -> SolrQuery Untyped.SolrExpr
    -> Maybe (SolrQuery Typed.SolrExpr)
  binop con u1 u2 = do
    q1 <- typeCheckSolrQuery u1
    q2 <- typeCheckSolrQuery u2
    pure (con q1 q2)

-- | Factor a Solr query into a canonical form (e.g. perform double-negation
-- elimination). Check the source code for all transformations performed.
factorSolrQuery :: SolrQuery expr -> SolrQuery expr
factorSolrQuery =
    transform rightAssocAppend
  . transform doubleNegationElim
  . transform elimInnerScores
 where
  -- Eliminate all scores inside of a scored query (essentially, the outermost
  -- score takes precedence).
  elimInnerScores :: SolrQuery expr -> SolrQuery expr
  elimInnerScores = \case
    QScore q n -> QScore (unscore q) n
    q          -> q
   where
    -- Because the outer transformation is bottom-up, we can stop at the first
    -- QScore we find (top-down).
    unscore :: SolrQuery expr -> SolrQuery expr
    unscore = \case
      QAnd q1 q2    -> QAnd (unscore q1) (unscore q2)
      QOr q1 q2     -> QOr (unscore q1) (unscore q2)
      QNot q1 q2    -> QNot (unscore q1) (unscore q2)
      QScore q _    -> q -- note, not 'unscore q'
      QNeg q        -> QNeg (unscore q)
      QAppend q1 q2 -> QAppend (unscore q1) (unscore q2)
      q             -> q

  -- Rewrite -(-q) as q
  doubleNegationElim :: SolrQuery expr -> SolrQuery expr
  doubleNegationElim = \case
    QNeg (QNeg q) -> q
    q -> q

  -- Rewrite ((q1 <> q2) <> q3) as (q1 <> (q2 <> q3))
  rightAssocAppend :: SolrQuery expr -> SolrQuery expr
  rightAssocAppend = \case
    QAppend (QAppend q1 q2) q3 -> QAppend q1 (QAppend q2 q3)
    q -> q


-- | Reinterpret an initially-encoded 'SolrQuery' to some other interpretation.
--
-- This may be useful for reinterpreting a 'SolrQuery' as a lazy
-- 'Data.Text.Lazy.Text' after it's been type checked and factored with the
-- machinery in this module.
reinterpretSolrQuery
  :: forall expr query.
     ( SolrQuerySYM expr query
     , Semigroup (query expr)
     )
  => SolrQuery Typed.SolrExpr
  -> query expr
reinterpretSolrQuery = fix $ \r -> \case
  QDefaultField e -> defaultField (Typed.reinterpretSolrExpr e)
  QField s e      -> field s (Typed.reinterpretSolrExpr e)
  QAnd q1 q2      -> r q1 &&: r q2
  QOr q1 q2       -> r q1 ||: r q2
  QNot q1 q2      -> r q1 -: r q2
  QScore q n      -> r q ^=: n
  QNeg q          -> neg (r q)
  QAppend q1 q2 -> r q1 <> r q2

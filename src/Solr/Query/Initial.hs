{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | An initial encoding of a Solr query. This is an alternative interpretation
-- of the Solr language that is more amenable to parsing from arbitrary user
-- input.

module Solr.Query.Initial
  ( -- * Query type
    SolrQuery(..)
  , ParamKey(..)
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

import qualified Solr.Expr.Initial.Untyped as Untyped
import qualified Solr.Expr.Initial.Typed   as Typed

import Data.Constraint               ((:-), (\\))
import Data.Constraint.Forall
import Data.Function                 (fix)
import Data.Generics.Uniplate.Direct (Uniplate(..), (|-), (|*), plate, rewrite, transform)
import Data.Generics.Str             (Str)
import Data.Semigroup                (Semigroup(..))
import Data.Text (Text)
import GHC.Show                      (showSpace)


-- | An initial encoding of a Solr query.
data SolrQuery expr
  = forall a. QDefaultField (expr a)
  | forall a. QField Text (expr a)
  | QAnd (SolrQuery expr) (SolrQuery expr)
  | QOr (SolrQuery expr) (SolrQuery expr)
  | QNot (SolrQuery expr) (SolrQuery expr)
  | QScore (SolrQuery expr) Float
  | QNeg (SolrQuery expr)
  | QParams [Param SolrQuery] (SolrQuery expr)
  | QAppend (SolrQuery expr) (SolrQuery expr)

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
    QParams ps q  -> showParen (n >= 11) (showString "QParams " . showsPrec 11 ps . showSpace . showsPrec 11 q)
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
    QParams ps q    -> plate QParams |- ps |* q
    QAppend q1 q2   -> plate QAppend |* q1 |* q2

instance SolrExprSYM expr => SolrQuerySYM expr SolrQuery where
  data ParamKey SolrQuery a where
    SolrQueryDefaultField :: ParamKey SolrQuery Text
    SolrQueryOp           :: ParamKey SolrQuery Text

  defaultField = QDefaultField
  (=:)         = QField
  (&&:)        = QAnd
  (||:)        = QOr
  (-:)         = QNot
  (^=:)        = QScore
  neg          = QNeg
  params       = QParams

instance Show (Param SolrQuery) where
  showsPrec n = \case
    Param SolrQueryDefaultField s ->
      showParen (n >= 11) (showString "Param SolrQueryDefaultField " . showSpace . showsPrec 11 s)
    Param SolrQueryOp s ->
      showParen (n >= 11) (showString "Param SolrQueryOp " . showSpace . showsPrec 11 s)

instance HasParamDefaultField SolrQuery where
  paramDefaultField = SolrQueryDefaultField

instance HasParamOp SolrQuery where
  paramOp = SolrQueryOp


-- | Type check an untyped Solr query. Note the 'Untyped.SolrExpr' on the way in
-- is not the same as the 'Typed.SolrExpr' on the way out.
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

    QParams ps u -> do
      q <- typeCheckSolrQuery u
      pure (QParams ps q)

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

-- | Attempt to factor a Solr query into a canonical form that irons out invalid
-- queries that are not caught by the type system (for example, double-negation,
-- or multiple nested applications of 'params').
--
-- Check the source code for all transformations performed.
factorSolrQuery :: SolrQuery expr -> SolrQuery expr
factorSolrQuery =
    transform rightAssocAppend
  . transform doubleNegationElim
  . transform elimInnerScores
  . rewrite pushUpParams
 where
  -- Push all 'params' up to a big list at the top level.
  pushUpParams :: SolrQuery expr -> Maybe (SolrQuery expr)
  pushUpParams = \case
    QAnd (QParams ps q1) q2    -> Just (QParams ps (QAnd q1 q2))
    QAnd q1 (QParams ps q2)    -> Just (QParams ps (QAnd q1 q2))
    QOr (QParams ps q1) q2     -> Just (QParams ps (QOr q1 q2))
    QOr q1 (QParams ps q2)     -> Just (QParams ps (QOr q1 q2))
    QNot (QParams ps q1) q2    -> Just (QParams ps (QNot q1 q2))
    QNot q1 (QParams ps q2)    -> Just (QParams ps (QNot q1 q2))
    QScore (QParams ps q) n    -> Just (QParams ps (QScore q n))
    QNeg (QParams ps q)        -> Just (QParams ps (QNeg q))
    QParams ps (QParams ps' q) -> Just (QParams (ps ++ ps') q) -- TODO: Merge params
    QAppend (QParams ps q1) q2 -> Just (QParams ps (QAppend q1 q2))
    QAppend q1 (QParams ps q2) -> Just (QParams ps (QAppend q1 q2))
    _                          -> Nothing

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
      -- We shouldn't ever hit this case because we apply pushUpParams first
      QParams ps q  -> QParams ps (unscore q)
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


-- Reinterpret an initially-encoded 'SolrQuery' to some other interpretation
-- that supports all of 'SolrQuery'\'s params.
--
-- This may be useful for reinterpreting a 'SolrQuery' as a lazy
-- 'Data.ByteString.Lazy.ByteString' after it's been type checked and factored
-- with the machinery in this module.
reinterpretSolrQuery
  :: forall expr query.
     ( SolrQuerySYM expr query
     , HasParamDefaultField query
     , HasParamOp query
     , Semigroup (query expr)
     )
  => SolrQuery expr
  -> query expr
reinterpretSolrQuery = fix $ \r -> \case
  QDefaultField e -> defaultField e
  QField s e      -> field s e
  QAnd q1 q2      -> r q1 &&: r q2
  QOr q1 q2       -> r q1 ||: r q2
  QNot q1 q2      -> r q1 -: r q2
  QScore q n      -> r q ^=: n
  QNeg q          -> neg (r q)
  QParams ps q    -> params (map f ps) (r q)
   where
    f :: Param SolrQuery -> Param query
    f (Param k s) =
      case k of
        SolrQueryDefaultField -> paramDefaultField .= s
        SolrQueryOp -> paramOp .= s
  QAppend q1 q2   -> r q1 <> r q2

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

-- | An initial encoding of a Solr query. This is an alternative interpretation
-- of the Solr language that is more amenable to parsing from arbitrary user
-- input.
--
-- Warning: This module is not very complete, or tested.

module Solr.Query.Initial
  (
  -- * Query type
    SolrQuery(..)
  -- * Expression type
  , SolrExpr(..)
  -- * Query compilation
  , solrQuery
  -- * Type checking
  , typeCheckSolrQuery
  ) where

import Solr.Class
import Solr.Param
import Solr.Type

import qualified Solr.Query.Initial.Untyped as Untyped

import Data.Text (Text)

-- | A Solr expression.
data SolrExpr ty where
  ENum    :: Float -> SolrExpr 'TNum
  ETrue   :: SolrExpr 'TBool
  EFalse  :: SolrExpr 'TBool
  EWord   :: Text -> SolrExpr 'TWord
  EWild   :: Text -> SolrExpr 'TWild
  ERegex  :: Text -> SolrExpr 'TRegex
  EPhrase :: [SolrExpr 'TWord] -> SolrExpr 'TPhrase
  EFuzz   :: FuzzableType a => SolrExpr a -> Int -> SolrExpr 'TFuzzed
  ETo     :: PrimType a => Boundary (SolrExpr a) -> Boundary (SolrExpr a) -> SolrExpr 'TRange
  EBoost  :: BoostableType a => SolrExpr a -> Float -> SolrExpr 'TBoosted

instance HasSolrType SolrExpr where
  getSolrType (ENum _)     = STNum
  getSolrType ETrue        = STBool
  getSolrType EFalse       = STBool
  getSolrType (EWord _)    = STWord
  getSolrType (EWild _)    = STWild
  getSolrType (ERegex _)   = STRegex
  getSolrType (EPhrase _)  = STPhrase
  getSolrType (EFuzz _ _)  = STFuzzed
  getSolrType (ETo _ _)    = STRange
  getSolrType (EBoost _ _) = STBoosted

instance SolrExprSYM SolrExpr where
  num    = ENum
  true   = ETrue
  false  = EFalse
  word   = EWord
  wild   = EWild
  regex  = ERegex
  phrase = EPhrase
  (~:)   = EFuzz
  to     = ETo
  (^:)   = EBoost


-- | A Solr query.
data SolrQuery where
  QDefaultField :: SolrExpr a -> SolrQuery
  QField        :: Text -> SolrExpr a -> SolrQuery
  QAnd          :: SolrQuery -> SolrQuery -> SolrQuery
  QOr           :: SolrQuery -> SolrQuery -> SolrQuery
  QNot          :: SolrQuery -> SolrQuery -> SolrQuery
  QScore        :: SolrQuery -> Float -> SolrQuery
  QNeg          :: SolrQuery -> SolrQuery
  QParams       :: [Param SolrQuery] -> SolrQuery -> SolrQuery

instance SolrQuerySYM SolrExpr SolrQuery where
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

instance HasParamDefaultField SolrQuery where
  paramDefaultField = SolrQueryDefaultField

instance HasParamOp SolrQuery where
  paramOp = SolrQueryOp


-- | Select the 'SolrQuery' interpreter.
--
-- > solrQuery = id
solrQuery :: SolrQuery -> SolrQuery
solrQuery = id


typeCheckSolrExpr :: Untyped.SolrExpr a -> r -> (forall ty. SolrExpr ty -> r) -> r
typeCheckSolrExpr u0 die k =
  case u0 of
    Untyped.ENum n -> k (ENum n)

    Untyped.ETrue -> k ETrue

    Untyped.EFalse -> k EFalse

    Untyped.EWord s -> k (EWord s)

    Untyped.EWild s -> k (EWild s)

    Untyped.ERegex s -> k (ERegex s)

    Untyped.EPhrase ss0 -> go [] ss0
     where
      go acc [] = k (EPhrase (reverse acc))
      go acc (s:ss) =
        typeCheckSolrExpr s die
          (\e ->
            case getSolrType e of
              STWord -> go (e:acc) ss
              _      -> die)
    Untyped.EFuzz u n ->
      typeCheckSolrExpr u die
        (\e ->
          case getSolrType e of
            STWord   -> k (EFuzz e n)
            STPhrase -> k (EFuzz e n)
            _        -> die)

    -- Hm, when typechecking a [* TO *], do I really have to just pick a type
    -- here? Seems wrong...
    Untyped.ETo Star Star ->
      k (ETo (Star :: Boundary (SolrExpr 'TNum)) Star)

    Untyped.ETo Star (Inclusive u) -> starLeft  Inclusive u die k
    Untyped.ETo Star (Exclusive u) -> starLeft  Exclusive u die k
    Untyped.ETo (Inclusive u) Star -> starRight Inclusive u die k
    Untyped.ETo (Exclusive u) Star -> starRight Exclusive u die k

    Untyped.ETo (Inclusive u1) (Inclusive u2) -> noStar Inclusive Inclusive u1 u2 die k
    Untyped.ETo (Inclusive u1) (Exclusive u2) -> noStar Inclusive Exclusive u1 u2 die k
    Untyped.ETo (Exclusive u1) (Inclusive u2) -> noStar Exclusive Inclusive u1 u2 die k
    Untyped.ETo (Exclusive u1) (Exclusive u2) -> noStar Exclusive Exclusive u1 u2 die k

    Untyped.EBoost u n ->
      typeCheckSolrExpr u die
        (\e ->
          case getSolrType e of
            STWord   -> k (EBoost e n)
            STPhrase -> k (EBoost e n)
            _        -> die)

starLeft :: (forall x. x -> Boundary x) -> Untyped.SolrExpr a -> r -> (forall ty. SolrExpr ty -> r) -> r
starLeft con u die k =
  typeCheckSolrExpr u die
    (\e ->
      case getSolrType e of
        STNum  -> k (ETo Star (con e))
        STWord -> k (ETo Star (con e))
        _      -> die)

starRight :: (forall x. x -> Boundary x) -> Untyped.SolrExpr a -> r -> (forall ty. SolrExpr ty -> r) -> r
starRight con u die k =
  typeCheckSolrExpr u die
    (\e ->
      case getSolrType e of
        STNum  -> k (ETo (con e) Star)
        STWord -> k (ETo (con e) Star)
        _      -> die)

noStar
  :: (forall x. x -> Boundary x)
  -> (forall x. x -> Boundary x)
  -> Untyped.SolrExpr a
  -> Untyped.SolrExpr a
  -> r
  -> (forall ty. SolrExpr ty -> r)
  -> r
noStar con1 con2 u1 u2 die k =
  typeCheckSolrExpr u1 die
    (\e1 ->
      typeCheckSolrExpr u2 die
        (\e2 ->
          case (getSolrType e1, getSolrType e2) of
            (STNum,  STNum)  -> k (ETo (con1 e1) (con2 e2))
            (STWord, STWord) -> k (ETo (con1 e1) (con2 e2))
            _ -> die))


typeCheckSolrQuery :: Untyped.SolrQuery -> r -> (SolrQuery -> r) -> r
typeCheckSolrQuery u0 die k =
  case u0 of
    Untyped.QDefaultField u ->
      typeCheckSolrExpr u die
        (\e -> k (QDefaultField e))

    Untyped.QField s u ->
      typeCheckSolrExpr u die
        (\e -> k (QField s e))

    Untyped.QAnd u1 u2 -> binop QAnd u1 u2
    Untyped.QOr  u1 u2 -> binop QOr  u1 u2
    Untyped.QNot u1 u2 -> binop QNot u1 u2

    Untyped.QScore u n ->
      typeCheckSolrQuery u die
        (\q -> k (QScore q n))

    Untyped.QNeg u ->
      typeCheckSolrQuery u die
        (\q -> k (QNeg q))
 where
  binop con u1 u2 =
    typeCheckSolrQuery u1 die
      (\q1 ->
        typeCheckSolrQuery u2 die
          (\q2 -> k (con q1 q2)))

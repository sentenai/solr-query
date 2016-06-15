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
    SolrQueryI(..)
  -- * Expression type
  , SolrExprI(..)
  -- * Query compilation
  , solrQueryI
  -- * Untyped ADTs
  , USolrQueryI(..)
  , USolrExprI(..)
  -- * Type checking
  , typeCheckSolrQuery
  ) where

import Solr.Class
import Solr.Param
import Solr.Type

import Data.Text (Text)

-- | A Solr expression.
data SolrExprI ty where
  ENum    :: Float -> SolrExprI 'TNum
  ETrue   :: SolrExprI 'TBool
  EFalse  :: SolrExprI 'TBool
  EWord   :: Text -> SolrExprI 'TWord
  EWild   :: Text -> SolrExprI 'TWild
  ERegex  :: Text -> SolrExprI 'TRegex
  EPhrase :: [SolrExprI 'TWord] -> SolrExprI 'TPhrase
  EFuzz   :: FuzzableType a => SolrExprI a -> Int -> SolrExprI 'TFuzzed
  ETo     :: PrimType a => Boundary (SolrExprI a) -> Boundary (SolrExprI a) -> SolrExprI 'TRange
  EBoost  :: BoostableType a => SolrExprI a -> Float -> SolrExprI 'TBoosted

instance HasSolrType SolrExprI where
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

instance SolrExprSYM SolrExprI where
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
data SolrQueryI where
  QDefaultField :: SolrExprI a -> SolrQueryI
  QField        :: Text -> SolrExprI a -> SolrQueryI
  QAnd          :: SolrQueryI -> SolrQueryI -> SolrQueryI
  QOr           :: SolrQueryI -> SolrQueryI -> SolrQueryI
  QNot          :: SolrQueryI -> SolrQueryI -> SolrQueryI
  QScore        :: SolrQueryI -> Float -> SolrQueryI
  QNeg          :: SolrQueryI -> SolrQueryI
  QParams       :: [Param SolrQueryI] -> SolrQueryI -> SolrQueryI

instance SolrQuerySYM SolrExprI SolrQueryI where
  data ParamKey SolrQueryI a where
    SolrQueryIDefaultField :: ParamKey SolrQueryI Text
    SolrQueryIOp           :: ParamKey SolrQueryI Text

  defaultField = QDefaultField
  (=:)         = QField
  (&&:)        = QAnd
  (||:)        = QOr
  (-:)         = QNot
  (^=:)        = QScore
  neg          = QNeg
  params       = QParams

instance HasParamDefaultField SolrQueryI where
  paramDefaultField = SolrQueryIDefaultField

instance HasParamOp SolrQueryI where
  paramOp = SolrQueryIOp


-- | Select the 'SolrQueryI' interpreter.
--
-- > solrQueryI = id
solrQueryI :: SolrQueryI -> SolrQueryI
solrQueryI = id


-- | An untyped Solr expression.
data USolrExprI
  = UENum Float
  | UETrue
  | UEFalse
  | UEWord Text
  | UEWild Text
  | UERegex Text
  | UEPhrase [USolrExprI]
  | UEFuzz USolrExprI Int
  | UETo (Boundary USolrExprI) (Boundary USolrExprI)
  | UEBoost USolrExprI Float

typeCheckSolrExpr :: USolrExprI -> r -> (forall ty. SolrExprI ty -> r) -> r
typeCheckSolrExpr u0 die k =
  case u0 of
    UENum n -> k (ENum n)

    UETrue -> k ETrue

    UEFalse -> k EFalse

    UEWord s -> k (EWord s)

    UEWild s -> k (EWild s)

    UERegex s -> k (ERegex s)

    UEPhrase ss0 -> go [] ss0
     where
      go acc [] = k (EPhrase (reverse acc))
      go acc (s:ss) =
        typeCheckSolrExpr s die
          (\e ->
            case getSolrType e of
              STWord -> go (e:acc) ss
              _      -> die)
    UEFuzz u n ->
      typeCheckSolrExpr u die
        (\e ->
          case getSolrType e of
            STWord   -> k (EFuzz e n)
            STPhrase -> k (EFuzz e n)
            _        -> die)

    -- Hm, when typechecking a [* TO *], do I really have to just pick a type
    -- here? Seems wrong...
    UETo Star Star ->
      k (ETo (Star :: Boundary (SolrExprI 'TNum)) Star)

    UETo Star (Inclusive u) -> starLeft  Inclusive u die k
    UETo Star (Exclusive u) -> starLeft  Exclusive u die k
    UETo (Inclusive u) Star -> starRight Inclusive u die k
    UETo (Exclusive u) Star -> starRight Exclusive u die k

    UETo (Inclusive u1) (Inclusive u2) -> noStar Inclusive Inclusive u1 u2 die k
    UETo (Inclusive u1) (Exclusive u2) -> noStar Inclusive Exclusive u1 u2 die k
    UETo (Exclusive u1) (Inclusive u2) -> noStar Exclusive Inclusive u1 u2 die k
    UETo (Exclusive u1) (Exclusive u2) -> noStar Exclusive Exclusive u1 u2 die k

    UEBoost u n ->
      typeCheckSolrExpr u die
        (\e ->
          case getSolrType e of
            STWord   -> k (EBoost e n)
            STPhrase -> k (EBoost e n)
            _        -> die)

starLeft :: (forall a. a -> Boundary a) -> USolrExprI -> r -> (forall ty. SolrExprI ty -> r) -> r
starLeft con u die k =
  typeCheckSolrExpr u die
    (\e ->
      case getSolrType e of
        STNum  -> k (ETo Star (con e))
        STWord -> k (ETo Star (con e))
        _      -> die)

starRight :: (forall a. a -> Boundary a) -> USolrExprI -> r -> (forall ty. SolrExprI ty -> r) -> r
starRight con u die k =
  typeCheckSolrExpr u die
    (\e ->
      case getSolrType e of
        STNum  -> k (ETo (con e) Star)
        STWord -> k (ETo (con e) Star)
        _      -> die)

noStar
  :: (forall a. a -> Boundary a)
  -> (forall a. a -> Boundary a)
  -> USolrExprI
  -> USolrExprI
  -> r
  -> (forall ty. SolrExprI ty -> r)
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


-- | An untyped Solr query.
data USolrQueryI
  = UQDefaultField USolrExprI
  | UQField Text USolrExprI
  | UQAnd USolrQueryI USolrQueryI
  | UQOr USolrQueryI USolrQueryI
  | UQNot USolrQueryI USolrQueryI
  | UQScore USolrQueryI Float
  | UQNeg USolrQueryI
  -- TODO: Figure this out
  -- UQParams [Param SolrQueryI] USolrQueryI

typeCheckSolrQuery :: USolrQueryI -> r -> (SolrQueryI -> r) -> r
typeCheckSolrQuery u0 die k =
  case u0 of
    UQDefaultField u ->
      typeCheckSolrExpr u die
        (\e -> k (QDefaultField e))

    UQField s u ->
      typeCheckSolrExpr u die
        (\e -> k (QField s e))

    UQAnd u1 u2 -> binop QAnd u1 u2
    UQOr  u1 u2 -> binop QOr  u1 u2
    UQNot u1 u2 -> binop QNot u1 u2

    UQScore u n ->
      typeCheckSolrQuery u die
        (\q -> k (QScore q n))

    UQNeg u ->
      typeCheckSolrQuery u die
        (\q -> k (QNeg q))
 where
  binop con u1 u2 =
    typeCheckSolrQuery u1 die
      (\q1 ->
        typeCheckSolrQuery u2 die
          (\q2 -> k (con q1 q2)))

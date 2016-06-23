{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

-- | This module is not yet complete.

module Solr.Expr.Initial.Typed
  ( -- * Expression type
    SolrExpr(..)
    -- * Expression construction
  , num
  , true
  , false
  , word
  , wild
  , regex
  , phrase
  , (~:)
  , fuzz
  , fuzzy
  , to
  , incl
  , excl
  , star
  , gt
  , gte
  , lt
  , lte
  , (^:)
  , boost
    -- * Type checking
  , typeCheckSolrExpr
  ) where

import Solr.Internal.Class
import Solr.Type

import qualified Solr.Expr.Initial.Untyped as Untyped

import Data.Text (Text)


-- | A Solr expression.
data SolrExpr :: SolrType -> * where
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

-- | Typecheck an untyped Solr expression. Note the 'Untyped.SolrExpr' on the
-- way in is not the same as the 'Typed.SolrExpr' on the way out.
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

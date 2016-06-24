{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}

module Solr.Expr.Initial.Typed
  ( -- * Expression type
    SolrExpr(..)
    -- * Type checking
  , typeCheckSolrExpr
    -- * Re-exports
  , module Solr.Internal.Class.Expr
  ) where

import Solr.Internal.Class.Expr
import Solr.Type

import qualified Solr.Expr.Initial.Untyped.Internal as Untyped

import Control.Monad (forM)
import Data.Text     (Text)


-- | A Solr expression.
data SolrExpr :: SolrType -> * where
  ENum    :: Float -> SolrExpr 'TNum
  ETrue   :: SolrExpr 'TBool
  EFalse  :: SolrExpr 'TBool
  EWord   :: Text -> SolrExpr 'TWord
  EWild   :: Text -> SolrExpr 'TWild
  ERegex  :: Text -> SolrExpr 'TRegex
  EPhrase :: [SolrExpr 'TWord] -> SolrExpr 'TPhrase
  EFuzz   :: Fuzzable a => SolrExpr a -> Int -> SolrExpr ('TFuzzed a)
  ETo     :: Rangeable a => Boundary (SolrExpr a) -> Boundary (SolrExpr a) -> SolrExpr ('TRanged a)
  EBoost  :: Boostable a => SolrExpr a -> Float -> SolrExpr ('TBoosted a)

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


-- | Type check an untyped Solr expression. Note the 'Untyped.SolrExpr' on the
-- way in is not the same as the 'SolrExpr' on the way out.
typeCheckSolrExpr :: Untyped.SolrExpr a -> (forall ty. Maybe (SolrExpr ty) -> r) -> r
typeCheckSolrExpr u k =
  case typeCheckSolrExpr' u of
    Nothing       -> k Nothing
    Just (Some e) -> k (Just e)

data Some (f :: k -> *) = forall a. Some (f a)

-- "Helper" type checker, working in a nicer monad (Maybe as opposed to Cont).
-- But, we expose the continuation-based rank-2 interface rather than exporting
-- this one-off 'Some' GADT.
typeCheckSolrExpr' :: Untyped.SolrExpr a -> Maybe (Some SolrExpr)
typeCheckSolrExpr' u0 =
  case u0 of
    Untyped.ENum n   -> pure (Some (ENum n))
    Untyped.ETrue    -> pure (Some ETrue)
    Untyped.EFalse   -> pure (Some EFalse)
    Untyped.EWord s  -> pure (Some (EWord s))
    Untyped.EWild s  -> pure (Some (EWild s))
    Untyped.ERegex s -> pure (Some (ERegex s))

    Untyped.EPhrase ss0 -> do
      es <- forM ss0 (\s -> do
                       Some e@(EWord _) <- typeCheckSolrExpr' s
                       pure e)
      pure (Some (EPhrase es))

    Untyped.EFuzz u n -> do
      Some e <- typeCheckSolrExpr' u
      case e of
        EWord _   -> pure (Some (EFuzz e n))
        EPhrase _ -> pure (Some (EFuzz e n))
        _         -> Nothing

    -- Hm, when type checking a [* TO *], do I really have to just pick a type
    -- here? Seems wrong...
    Untyped.ETo Star Star ->
      pure (Some (ETo (Star :: Boundary (SolrExpr 'TNum)) Star))

    Untyped.ETo Star (Inclusive u)            -> starLeft  Inclusive u
    Untyped.ETo Star (Exclusive u)            -> starLeft  Exclusive u
    Untyped.ETo (Inclusive u) Star            -> starRight Inclusive u
    Untyped.ETo (Exclusive u) Star            -> starRight Exclusive u

    Untyped.ETo (Inclusive u1) (Inclusive u2) -> noStar Inclusive Inclusive u1 u2
    Untyped.ETo (Inclusive u1) (Exclusive u2) -> noStar Inclusive Exclusive u1 u2
    Untyped.ETo (Exclusive u1) (Inclusive u2) -> noStar Exclusive Inclusive u1 u2
    Untyped.ETo (Exclusive u1) (Exclusive u2) -> noStar Exclusive Exclusive u1 u2

    Untyped.EBoost u n -> do
      Some e <- typeCheckSolrExpr' u
      case e of
        EWord _   -> pure (Some (EBoost e n))
        EPhrase _ -> pure (Some (EBoost e n))
        _         -> Nothing

-- Type check a *-to-EXPR
starLeft :: (forall x. x -> Boundary x) -> Untyped.SolrExpr a -> Maybe (Some SolrExpr)
starLeft con u = do
  Some e <- typeCheckSolrExpr' u
  case e of
    ENum _  -> pure (Some (ETo Star (con e)))
    EWord _ -> pure (Some (ETo Star (con e)))
    _       -> Nothing

-- Type check a EXPR-to-*
starRight :: (forall x. x -> Boundary x) -> Untyped.SolrExpr a -> Maybe (Some SolrExpr)
starRight con u = do
  Some e <- typeCheckSolrExpr' u
  case e of
    ENum _  -> pure (Some (ETo (con e) Star))
    EWord _ -> pure (Some (ETo (con e) Star))
    _       -> Nothing

-- Type check a EXPR-to-EXPR
noStar
  :: (forall x. x -> Boundary x)
  -> (forall x. x -> Boundary x)
  -> Untyped.SolrExpr a
  -> Untyped.SolrExpr a
  -> Maybe (Some SolrExpr)
noStar con1 con2 u1 u2 = do
  Some e1 <- typeCheckSolrExpr' u1
  Some e2 <- typeCheckSolrExpr' u2
  case (e1, e2) of
    (ENum _,  ENum _)  -> pure (Some (ETo (con1 e1) (con2 e2)))
    (EWord _, EWord _) -> pure (Some (ETo (con1 e1) (con2 e2)))
    _                  -> Nothing

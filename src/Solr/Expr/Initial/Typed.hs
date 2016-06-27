{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}

module Solr.Expr.Initial.Typed
  ( -- * Expression type
    SolrExpr(..)
  , SomeSolrExpr(..)
    -- * Type checking
  , typeCheckSolrExpr
  , typeCheckSolrExpr'
    -- * Reinterpretation
  , reinterpretSolrExpr
    -- * Re-exports
  , module Solr.Internal.Class.Expr
  ) where

import Solr.Internal.Class.Expr
import Solr.Type

import qualified Solr.Expr.Initial.Untyped as Untyped

import Control.Monad (forM)
import Data.Text     (Text)


-- | A typed, initially-encoded Solr expression.
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


-- | Existential wrapper around 'SolrExpr'.
data SomeSolrExpr = forall ty. SomeSolrExpr (SolrExpr ty)


-- | Type check an untyped Solr expression. Note the untyped 'Untyped.SolrExpr'
-- on the way in is not the same as the typed 'SolrExpr' on the way out.
--
-- @
-- 'typeCheckSolrExpr' u k =
--   case 'typeCheckSolrExpr'' u of
--     Nothing -> k Nothing
--     Just ('SomeSolrExpr' e) -> k (Just e)
-- @
typeCheckSolrExpr :: Untyped.SolrExpr a -> (forall ty. Maybe (SolrExpr ty) -> r) -> r
typeCheckSolrExpr u k =
  case typeCheckSolrExpr' u of
    Nothing -> k Nothing
    Just (SomeSolrExpr e) -> k (Just e)

-- | Like 'typeCheckSolrExpr', but return an existential type rather than use
-- rank-2 continuation passing style, if you prefer.
typeCheckSolrExpr' :: Untyped.SolrExpr a -> Maybe SomeSolrExpr
typeCheckSolrExpr' u0 =
  case u0 of
    Untyped.ENum n   -> pure (SomeSolrExpr (ENum n))
    Untyped.ETrue    -> pure (SomeSolrExpr ETrue)
    Untyped.EFalse   -> pure (SomeSolrExpr EFalse)
    Untyped.EWord s  -> pure (SomeSolrExpr (EWord s))
    Untyped.EWild s  -> pure (SomeSolrExpr (EWild s))
    Untyped.ERegex s -> pure (SomeSolrExpr (ERegex s))

    Untyped.EPhrase ss0 -> do
      es <- forM ss0 (\s -> do
                       SomeSolrExpr e@(EWord _) <- typeCheckSolrExpr' s
                       pure e)
      pure (SomeSolrExpr (EPhrase es))

    Untyped.EFuzz u n -> do
      SomeSolrExpr e <- typeCheckSolrExpr' u
      case e of
        EWord _   -> pure (SomeSolrExpr (EFuzz e n))
        EPhrase _ -> pure (SomeSolrExpr (EFuzz e n))
        _         -> Nothing

    -- Hm, when type checking a [* TO *], do I really have to just pick a type
    -- here? Seems wrong...
    Untyped.ETo Star Star ->
      pure (SomeSolrExpr (ETo (Star :: Boundary (SolrExpr 'TNum)) Star))

    Untyped.ETo Star (Inclusive u)            -> starLeft  Inclusive u
    Untyped.ETo Star (Exclusive u)            -> starLeft  Exclusive u
    Untyped.ETo (Inclusive u) Star            -> starRight Inclusive u
    Untyped.ETo (Exclusive u) Star            -> starRight Exclusive u

    Untyped.ETo (Inclusive u1) (Inclusive u2) -> noStar Inclusive Inclusive u1 u2
    Untyped.ETo (Inclusive u1) (Exclusive u2) -> noStar Inclusive Exclusive u1 u2
    Untyped.ETo (Exclusive u1) (Inclusive u2) -> noStar Exclusive Inclusive u1 u2
    Untyped.ETo (Exclusive u1) (Exclusive u2) -> noStar Exclusive Exclusive u1 u2

    Untyped.EBoost u n -> do
      SomeSolrExpr e <- typeCheckSolrExpr' u
      case e of
        EWord _   -> pure (SomeSolrExpr (EBoost e n))
        EPhrase _ -> pure (SomeSolrExpr (EBoost e n))
        _         -> Nothing

-- Type check a *-to-EXPR
starLeft :: (forall x. x -> Boundary x) -> Untyped.SolrExpr a -> Maybe SomeSolrExpr
starLeft con u = do
  SomeSolrExpr e <- typeCheckSolrExpr' u
  case e of
    ENum _  -> pure (SomeSolrExpr (ETo Star (con e)))
    EWord _ -> pure (SomeSolrExpr (ETo Star (con e)))
    _       -> Nothing

-- Type check a EXPR-to-*
starRight :: (forall x. x -> Boundary x) -> Untyped.SolrExpr a -> Maybe SomeSolrExpr
starRight con u = do
  SomeSolrExpr e <- typeCheckSolrExpr' u
  case e of
    ENum _  -> pure (SomeSolrExpr (ETo (con e) Star))
    EWord _ -> pure (SomeSolrExpr (ETo (con e) Star))
    _       -> Nothing

-- Type check a EXPR-to-EXPR
noStar
  :: (forall x. x -> Boundary x)
  -> (forall x. x -> Boundary x)
  -> Untyped.SolrExpr a
  -> Untyped.SolrExpr a
  -> Maybe SomeSolrExpr
noStar con1 con2 u1 u2 = do
  SomeSolrExpr e1 <- typeCheckSolrExpr' u1
  SomeSolrExpr e2 <- typeCheckSolrExpr' u2
  case (e1, e2) of
    (ENum _,  ENum _)  -> pure (SomeSolrExpr (ETo (con1 e1) (con2 e2)))
    (EWord _, EWord _) -> pure (SomeSolrExpr (ETo (con1 e1) (con2 e2)))
    _                  -> Nothing


-- | Reinterpret a Solr expression.
reinterpretSolrExpr :: SolrExprSYM expr => SolrExpr ty -> expr ty
reinterpretSolrExpr = \case
  ENum n     -> num n
  ETrue      -> true
  EFalse     -> false
  EWord s    -> word s
  EWild s    -> wild s
  ERegex s   -> regex s
  EPhrase es -> phrase (map reinterpretSolrExpr es)
  EFuzz e n  -> reinterpretSolrExpr e ~: n
  ETo e1 e2  -> fmap reinterpretSolrExpr e1 `to` fmap reinterpretSolrExpr e2
  EBoost e n -> reinterpretSolrExpr e ^: n

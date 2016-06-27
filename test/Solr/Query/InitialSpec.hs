{-# LANGUAGE LambdaCase #-}

module Solr.Query.InitialSpec where

import Orphans ()
import Solr.Query.Initial
import Solr.Expr.Initial.Untyped

import Control.Monad.State
import Data.Generics.Uniplate.Direct
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen


spec :: Spec
spec = do
  describe "factorSolrQuery" $
    prop "pushes all params to the top" $ \(QueryWithParams q) ->
      case factorSolrQuery q of
        QParams _ q' -> null (filter isParams (universe q'))
         where
          isParams (QParams _ _) = True
          isParams _ = False
        _ -> error "incorrect instance Arbitrary QueryWithParams"


--------------------------------------------------------------------------------

-- Newtype around SolrQuery whose arbitrary instance is guaranteed to include
-- a QParams node somewhere.
newtype QueryWithParams = QueryWithParams (SolrQuery SolrExpr)
  deriving Show

instance Arbitrary QueryWithParams where
  arbitrary = QueryWithParams <$> evalStateT go False
   where
    -- Arbitrary instance that keeps track of whether or not we've generated a
    -- QParams node yet.
    go :: StateT Bool Gen (SolrQuery SolrExpr)
    go = do
      get >>= \case
        -- If we have, delegate to underlying Arbitrary instance.
        True -> lift arbitrary
        -- If we haven't, first pick some constructor that itself contains a
        -- SolrQuery (QScore, QNeg, QParams, QAnd, QOr, QNot). Then, fill in
        -- its body, using either the underlying arbitrary instance, or *this*
        -- arbitrary instance, depending on if we've generated a QParams node
        -- yet.
        False ->
          lift arbitrary >>= \case
            PickScore -> do
              QueryWithParams q <- lift (scale (`div` 2) arbitrary)
              n <- lift arbitrary
              pure (QScore q n)
            PickNeg -> do
              QueryWithParams q <- lift (scale (`div` 2) arbitrary)
              pure (QNeg q)
            PickParams -> do
              put True
              lift (QParams <$> arbitrary <*> scale (`div` 2) arbitrary)
            PickAnd    -> binop QAnd
            PickOr     -> binop QOr
            PickNot    -> binop QNot
            PickAppend -> binop QAppend

    binop
      :: (SolrQuery SolrExpr -> SolrQuery SolrExpr -> SolrQuery SolrExpr)
      -> StateT Bool Gen (SolrQuery SolrExpr)
    binop f = do
      q1 <- lift arbitrary
      get >>= \case
        True -> do
          q2 <- lift arbitrary
          pure (f q1 q2)
        False -> do
          QueryWithParams q2 <- lift arbitrary
          pure (f q1 q2)

-- Pick which SolrQuery constructor?
data Pick
  = PickScore
  | PickNeg
  | PickParams
  | PickAnd
  | PickOr
  | PickNot
  | PickAppend

instance Arbitrary Pick where
  arbitrary = frequency
    [ (35, elements [PickScore, PickNeg, PickParams])
    , (25, elements [PickAnd, PickOr, PickNot, PickAppend])
    ]

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

import Solr.Query.Initial
import Solr.Expr.Initial.Untyped

import Data.Coerce               (coerce)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances ()


instance Arbitrary a => Arbitrary (Boundary a) where
  arbitrary = oneof
    [ Inclusive <$> arbitrary
    , Exclusive <$> arbitrary
    , pure Star
    ]

  shrink = \case
    Inclusive x -> [ Inclusive x' | x' <- shrink x ]
    Exclusive x -> [ Exclusive x' | x' <- shrink x ]
    Star        -> []

instance Arbitrary (Expr a) where
  arbitrary = frequency
    [ (40, oneof
        [ ENum <$> arbitrary
        , pure ETrue
        , pure EFalse
        , EWord <$> arbitrary
        , EWild <$> arbitrary
        , ERegex <$> arbitrary
        , EDateTime <$> arbitrary
        ])
    , (35, oneof
        [ EFuzz <$> scaleSub1 arbitrary <*> arbitrary
        , EBoost <$> scaleSub1 arbitrary <*> arbitrary
        ])
    , (25, oneof
        [ EPhrase <$> listOf (scale (`div` 3) arbitrary)
        , ETo <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
        ])
    ]

  shrink = \case
    ENum n      -> [ ENum n' | n' <- shrink n ]
    ETrue       -> []
    EFalse      -> []
    EWord s     -> [ EWord s' | s' <- shrink s ]
    EWild s     -> [ EWild s' | s' <- shrink s ]
    ERegex s    -> [ ERegex s' | s' <- shrink s ]
    EDateTime _ -> []
    EPhrase es  -> coerce es ++ [ EPhrase es' | es' <- shrink es ]
    EFuzz e n   -> coerce e : [ EFuzz e' n' | (e', n') <- shrink (e, n) ]
    ETo e1 e2   -> [ ETo e1' e2' | (e1', e2') <- shrink (e1, e2) ]
    EBoost e n  -> coerce e : [ EBoost e' n' | (e', n') <- shrink (e, n) ]

instance Arbitrary (Query Expr) where
  arbitrary = frequency
    [ (40, oneof
        [ QDefaultField <$> arbitrary
        , QField <$> arbitrary <*> arbitrary
        ])
    , (35, oneof
        [ QScore <$> scale (`div` 2) arbitrary <*> arbitrary
        , QNeg <$> scale (`div` 2) arbitrary
        ])
    , (25, oneof
        [ QAnd    <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
        , QOr     <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
        , QNot    <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
        , QAppend <$> scale (`div` 2) arbitrary <*> scale (`div` 2) arbitrary
        ])
    ]

  shrink = \case
    QDefaultField e -> [ QDefaultField e' | e' <- shrink e ]
    QField s e      -> [ QField s' e' | (s', e') <- shrink (s, e) ]
    QAnd q1 q2      -> q1 : q2 : [ QAnd q1' q2' | (q1', q2') <- shrink (q1, q2) ]
    QOr q1 q2       -> q1 : q2 : [ QOr q1' q2' | (q1', q2') <- shrink (q1, q2) ]
    QNot q1 q2      -> q1 : q2 : [ QNot q1' q2' | (q1', q2') <- shrink (q1, q2) ]
    QScore q n      -> q : [ QScore q' n' | (q', n') <- shrink (q, n) ]
    QNeg q          -> q : [ QNeg q' | q' <- shrink q ]
    QAppend q1 q2   -> q1 : q2 : [ QAppend q1' q2' | (q1', q2') <- shrink (q1, q2) ]

instance Arbitrary (Param Query) where
  arbitrary = oneof
    [ paramDefaultField <$> arbitrary
    , pure paramOpAnd
    , pure paramOpOr
    ]

-- Subtract by 1, but don't go below 0
scaleSub1 :: Gen a -> Gen a
scaleSub1 g = scale (max 0 . subtract 1) g

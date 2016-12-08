{-# LANGUAGE CPP                  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

import Solr.Query.Initial

import Data.Coerce (coerce)
import Prelude
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances ()

instance Arbitrary (UExpr a) where
  arbitrary = frequency
    [ (40, oneof
        [ UInt <$> arbitrary
        , UFloat <$> arbitrary
        , pure UTrue
        , pure UFalse
        , UWord <$> arbitrary
        , UWild <$> arbitrary
        , URegex <$> arbitrary
        , UDateTime <$> arbitrary
        ])
    , (35, oneof
        [ UFuzz <$> scaleSub1 arbitrary <*> arbitrary
        , UBoost <$> scaleSub1 arbitrary <*> arbitrary
        ])
    , (25, oneof
        [ UPhrase <$> listOf (scale (`div` 3) arbitrary)
        , UTo
            <$> scale (`div` 2) (oneof
                  [ Inclusive <$> arbitrary
                  , Exclusive <$> arbitrary
                  , pure Star
                  ])
            <*> scale (`div` 2) (oneof
                  [ Inclusive <$> arbitrary
                  , Exclusive <$> arbitrary
                  , pure Star
                  ])
        ])
    ]

  shrink = \case
    UInt n      -> [ UInt n' | n' <- shrink n ]
    UFloat n    -> [ UFloat n' | n' <- shrink n ]
    UTrue       -> []
    UFalse      -> []
    UWord s     -> [ UWord s' | s' <- shrink s ]
    UWild s     -> [ UWild s' | s' <- shrink s ]
    URegex s    -> [ URegex s' | s' <- shrink s ]
    UUTCTime _  -> []
    UDateTime _ -> []
    UPhrase es  -> coerce es ++ [ UPhrase es' | es' <- shrink es ]
    UFuzz e n   -> coerce e : [ UFuzz e' n' | (e', n') <- shrink (e, n) ]
    UTo e1 e2   ->
      case (e1, e2) of
        (Inclusive e1', Inclusive e2') -> [ UTo (Inclusive e1'') (Inclusive e2'') | (e1'', e2'') <- shrink (e1', e2') ]
        (Inclusive e1', Exclusive e2') -> [ UTo (Inclusive e1'') (Exclusive e2'') | (e1'', e2'') <- shrink (e1', e2') ]
        (Inclusive e1', Star)          -> [ UTo (Inclusive e1'') Star             | e1''         <- shrink e1' ]
        (Exclusive e1', Inclusive e2') -> [ UTo (Exclusive e1'') (Inclusive e2'') | (e1'', e2'') <- shrink (e1', e2') ]
        (Exclusive e1', Exclusive e2') -> [ UTo (Exclusive e1'') (Exclusive e2'') | (e1'', e2'') <- shrink (e1', e2') ]
        (Exclusive e1', Star)          -> [ UTo (Exclusive e1'') Star             | e1''         <- shrink e1' ]
        (Star,          Inclusive e2') -> [ UTo Star             (Inclusive e2'') | e2''         <- shrink e2' ]
        (Star,          Exclusive e2') -> [ UTo Star             (Exclusive e2'') | e2''         <- shrink e2' ]
        (Star,          Star)          -> []
    UBoost e n  -> coerce e : [ UBoost e' n' | (e', n') <- shrink (e, n) ]

instance Arbitrary (Q UExpr) where
  arbitrary = frequency
    [ (40, oneof
        [ QDefaultField <$> arbitrary
        , QField <$> arbitrary <*> arbitrary
        ])
    , (35, QScore <$> scale (`div` 2) arbitrary <*> arbitrary)
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
    QAppend q1 q2   -> q1 : q2 : [ QAppend q1' q2' | (q1', q2') <- shrink (q1, q2) ]

instance Arbitrary (LocalParam Q) where
  arbitrary = oneof
    [ df <$> arbitrary
    , pure opAnd
    , pure opOr
    ]

-- Subtract by 1, but don't go below 0
scaleSub1 :: Gen a -> Gen a
scaleSub1 g = scale (max 0 . subtract 1) g

#if !MIN_VERSION_QuickCheck(2,8,0)
scale :: (Int -> Int) -> Gen a -> Gen a
scale f g = sized (\n -> resize (f n) g)
#endif

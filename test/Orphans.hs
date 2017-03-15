{-# LANGUAGE CPP                  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans where

-- import Solr.Query.Lucene
--
-- import Data.Coerce (coerce)
-- import Prelude.Compat
-- import Test.QuickCheck.Arbitrary
-- import Test.QuickCheck.Gen
-- import Test.QuickCheck.Instances ()
--
-- instance Arbitrary (LocalParam LuceneQuerySYM) where
--   arbitrary = oneof
--     [ df <$> arbitrary
--     , pure opAnd
--     , pure opOr
--     ]
--
-- -- Subtract by 1, but don't go below 0
-- scaleSub1 :: Gen a -> Gen a
-- scaleSub1 g = scale (max 0 . subtract 1) g
--
-- #if !MIN_VERSION_QuickCheck(2,8,0)
-- scale :: (Int -> Int) -> Gen a -> Gen a
-- scale f g = sized (\n -> resize (f n) g)
-- #endif

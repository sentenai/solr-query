{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Lucene.Type where


-- | A Lucene type.
data LuceneType
  = TInt
  | TBool
  | TWord
  | TFuzzyWord
  | TBoostedWord
  | TWild
  | TRegex
  | TPhrase
  | TFuzzyPhrase
  | TBoostedPhrase
  | TRange


-- | Types that can be fuzzed by a @\'~\'@ operator.
class FuzzableType (a :: LuceneType) where
  type TFuzzed a :: LuceneType

-- | @type TFuzzed TWord = TFuzzyWord@
instance FuzzableType 'TWord where
  type TFuzzed 'TWord = 'TFuzzyWord

-- | @type TFuzzed TPhrase = TFuzzyPhrase@
instance FuzzableType 'TPhrase where
  type TFuzzed 'TPhrase = 'TFuzzyPhrase


-- | Types that can be boosted by a @\'^\'@ operator.
class BoostableType (a :: LuceneType) where
  type TBoosted a :: LuceneType

-- | @type TBoosted TWord = TBoostedWord@
instance BoostableType 'TWord where
  type TBoosted 'TWord = 'TBoostedWord

-- | @type TBoosted TPhrase = TBoostedPhrase@
instance BoostableType 'TPhrase where
  type TBoosted 'TPhrase = 'TBoostedPhrase


-- | Types that can appear in a range expression.
class PrimType (a :: LuceneType)

instance PrimType 'TWord
instance PrimType 'TInt

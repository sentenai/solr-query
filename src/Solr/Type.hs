{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Solr.Type where


-- | A Solr type.
data SolrType
  = TNum
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
class FuzzableType (a :: SolrType) where
  type TFuzzed a :: SolrType

-- | @type TFuzzed TWord = TFuzzyWord@
instance FuzzableType 'TWord where
  type TFuzzed 'TWord = 'TFuzzyWord

-- | @type TFuzzed TPhrase = TFuzzyPhrase@
instance FuzzableType 'TPhrase where
  type TFuzzed 'TPhrase = 'TFuzzyPhrase


-- | Types that can be boosted by a @\'^\'@ operator.
class BoostableType (a :: SolrType) where
  type TBoosted a :: SolrType

-- | @type TBoosted TWord = TBoostedWord@
instance BoostableType 'TWord where
  type TBoosted 'TWord = 'TBoostedWord

-- | @type TBoosted TPhrase = TBoostedPhrase@
instance BoostableType 'TPhrase where
  type TBoosted 'TPhrase = 'TBoostedPhrase


-- | Types that can appear in a range expression.
class PrimType (a :: SolrType)

instance PrimType 'TNum
instance PrimType 'TWord

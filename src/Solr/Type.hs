{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Solr.Type where


-- | A Solr type.
data SolrType
  = TNum
  | TBool
  | TWord
  | TWild
  | TRegex
  | TPhrase
  | TFuzzed
  | TBoosted
  | TRange

-- | A Solr type singleton.
data SSolrType :: SolrType -> * where
  STNum     :: SSolrType 'TNum
  STBool    :: SSolrType 'TBool
  STWord    :: SSolrType 'TWord
  STWild    :: SSolrType 'TWild
  STRegex   :: SSolrType 'TRegex
  STPhrase  :: SSolrType 'TPhrase
  STFuzzed  :: SSolrType 'TFuzzed
  STBoosted :: SSolrType 'TBoosted
  STRange   :: SSolrType 'TRange


-- | Types that can be fuzzed by a @\'~\'@ operator.
class FuzzableType (a :: SolrType)

instance FuzzableType 'TWord
instance FuzzableType 'TPhrase


-- | Types that can be boosted by a @\'^\'@ operator.
class BoostableType (a :: SolrType)

instance BoostableType 'TWord
instance BoostableType 'TPhrase


-- | Types that can appear in a range expression.
class PrimType (a :: SolrType)

instance PrimType 'TNum
instance PrimType 'TWord

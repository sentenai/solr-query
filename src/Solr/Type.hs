{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TypeFamilies    #-}

module Solr.Type
  ( -- * Solr types
    SolrType(..)
  , Fuzzable
  , Boostable
  , Rangeable
  ) where


data SolrType
  = TNum
  | TBool
  | TWord
  | TWild
  | TRegex
  | TPhrase
  | TFuzzed SolrType
  | TBoosted SolrType
  | TRanged SolrType

-- | Types that can be fuzzed by a @\'~\'@ operator.
class Fuzzable (ty :: SolrType)
instance Fuzzable 'TWord
instance Fuzzable 'TPhrase

-- | Types that can be boosted by a @\'^\'@ operator.
class Boostable (ty :: SolrType)
instance Boostable 'TWord
instance Boostable 'TPhrase

-- | Types that can appear in a @'TO'@ range expression.
class Rangeable (ty :: SolrType)
instance Rangeable 'TNum
instance Rangeable 'TWord

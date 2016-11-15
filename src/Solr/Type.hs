{-# language CPP                                #-}
{-# language UndecidableInstances               #-}
{-# options_ghc -fno-warn-redundant-constraints #-}

module Solr.Type
  ( -- * Solr types
    SolrType(..)
  , Fuzzable
  , Boostable
  , Rangeable
  ) where

#if MIN_VERSION_base(4,9,0)
import GHC.TypeLits (TypeError, ErrorMessage(..))
#endif

data SolrType
  = TNum
  | TBool
  | TWord
  | TWild
  | TRegex
  | TPhrase
  | TDateTime
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
instance Rangeable 'TDateTime

#if MIN_VERSION_base(4,9,0)
instance TypeError ('Text "You can only fuzz words and phrases") => Fuzzable 'TNum
instance TypeError ('Text "You can only fuzz words and phrases") => Fuzzable 'TBool
instance TypeError ('Text "You can only fuzz words and phrases") => Fuzzable 'TWild
instance TypeError ('Text "You can only fuzz words and phrases") => Fuzzable 'TRegex
instance TypeError ('Text "You can only fuzz words and phrases") => Fuzzable 'TDateTime
instance TypeError ('Text "You can only fuzz words and phrases") => Fuzzable ('TFuzzed ty)
instance TypeError ('Text "You can only fuzz words and phrases") => Fuzzable ('TBoosted ty)
instance TypeError ('Text "You can only fuzz words and phrases") => Fuzzable ('TRanged ty)

instance TypeError ('Text "You can only boost words and phrases") => Boostable 'TNum
instance TypeError ('Text "You can only boost words and phrases") => Boostable 'TBool
instance TypeError ('Text "You can only boost words and phrases") => Boostable 'TWild
instance TypeError ('Text "You can only boost words and phrases") => Boostable 'TRegex
instance TypeError ('Text "You can only boost words and phrases") => Boostable 'TDateTime
instance TypeError ('Text "You can only boost words and phrases") => Boostable ('TFuzzed ty)
instance TypeError ('Text "You can only boost words and phrases") => Boostable ('TBoosted ty)
instance TypeError ('Text "You can only boost words and phrases") => Boostable ('TRanged ty)

instance TypeError ('Text "You can only use numbers, words, and dates in a range expression") => Rangeable 'TBool
instance TypeError ('Text "You can only use numbers, words, and dates in a range expression") => Rangeable 'TWild
instance TypeError ('Text "You can only use numbers, words, and dates in a range expression") => Rangeable 'TRegex
instance TypeError ('Text "You can only use numbers, words, and dates in a range expression") => Rangeable 'TPhrase
instance TypeError ('Text "You can only use numbers, words, and dates in a range expression") => Rangeable ('TFuzzed ty)
instance TypeError ('Text "You can only use numbers, words, and dates in a range expression") => Rangeable ('TBoosted ty)
instance TypeError ('Text "You can only use numbers, words, and dates in a range expression") => Rangeable ('TRanged ty)
#endif

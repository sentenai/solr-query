{-# language CPP                  #-}
{-# language UndecidableInstances #-}

#if MIN_VERSION_base(4,9,0)
{-# options_ghc -fno-warn-redundant-constraints #-}
#endif

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
  = TAny
  | TNum
  | TBool
  | TWord
  | TWild
  | TRegex
  | TPhrase
  | TDateTime
  | TFuzzy
  | TBoosted
  | TRange

-- | Types that can be fuzzed by a @\'~\'@ operator.
class Fuzzable (ty :: SolrType)
instance Fuzzable 'TWord
instance Fuzzable 'TPhrase

-- | Types that can be boosted by a @\'^\'@ operator.
class Boostable (ty :: SolrType)
instance Boostable 'TWord
instance Boostable 'TPhrase

-- | Types that can appear in a @'TO'@ range expression.
class Rangeable (a :: SolrType) (b :: SolrType)
instance Rangeable 'TNum      'TNum
instance Rangeable 'TNum      'TAny
instance Rangeable 'TWord     'TWord
instance Rangeable 'TWord     'TAny
instance Rangeable 'TDateTime 'TDateTime
instance Rangeable 'TDateTime 'TAny
instance Rangeable 'TAny      'TNum
instance Rangeable 'TAny      'TWord
instance Rangeable 'TAny      'TDateTime
instance Rangeable 'TAny      'TAny

#if MIN_VERSION_base(4,9,0)
instance {-# OVERLAPPABLE #-} TypeError ('Text "You can only fuzz words and phrases") => Fuzzable a
instance {-# OVERLAPPABLE #-} TypeError ('Text "You can only boost words and phrases") => Boostable a
instance {-# OVERLAPPABLE #-} TypeError ('Text "You can only use numbers, words, and dates in a range expression") => Rangeable a b
#endif

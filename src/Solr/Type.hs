{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TypeFamilies    #-}

module Solr.Type
  (
  -- * Solr types
    SolrType(..)
  , SSolrType(..)
  , HasSolrType(..)
  , FuzzableType
  , BoostableType
  , PrimType
  ) where

import GHC.Exts (Constraint)


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
data SSolrType ty where
  STNum     :: SSolrType 'TNum
  STBool    :: SSolrType 'TBool
  STWord    :: SSolrType 'TWord
  STWild    :: SSolrType 'TWild
  STRegex   :: SSolrType 'TRegex
  STPhrase  :: SSolrType 'TPhrase
  STFuzzed  :: SSolrType 'TFuzzed
  STBoosted :: SSolrType 'TBoosted
  STRange   :: SSolrType 'TRange


-- | Structures that are tagged with a Solr type.
class HasSolrType expr where
  getSolrType :: expr ty -> SSolrType ty


-- | Types that can be fuzzed by a @\'~\'@ operator.
type family FuzzableType ty :: Constraint where
  FuzzableType 'TWord   = ()
  FuzzableType 'TPhrase = ()


-- | Types that can be boosted by a @\'^\'@ operator.
type family BoostableType ty :: Constraint where
  BoostableType 'TWord   = ()
  BoostableType 'TPhrase = ()


-- | Types that can appear in a range expression.
type family PrimType ty :: Constraint where
  PrimType 'TNum  = ()
  PrimType 'TWord = ()

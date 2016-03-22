{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TypeFamilies    #-}

module Solr.Type
  (
  -- * Solr types
    TNum
  , TBool
  , TWord
  , TWild
  , TRegex
  , TPhrase
  , TFuzzed
  , TBoosted
  , TRange
  , SSolrType(..)
  , SolrType(..)
  , HasSolrType(..)
  , FuzzableType
  , BoostableType
  , PrimType
  ) where

import GHC.Exts (Constraint)


data TNum
data TBool
data TWord
data TWild
data TRegex
data TPhrase
data TFuzzed
data TBoosted
data TRange

-- | A Solr type singleton.
data SSolrType ty where
  STNum     :: SSolrType TNum
  STBool    :: SSolrType TBool
  STWord    :: SSolrType TWord
  STWild    :: SSolrType TWild
  STRegex   :: SSolrType TRegex
  STPhrase  :: SSolrType TPhrase
  STFuzzed  :: SSolrType TFuzzed
  STBoosted :: SSolrType TBoosted
  STRange   :: SSolrType TRange

class SolrType ty where
  solrType :: SSolrType ty

instance SolrType TNum     where solrType = STNum
instance SolrType TBool    where solrType = STBool
instance SolrType TWord    where solrType = STWord
instance SolrType TWild    where solrType = STWild
instance SolrType TRegex   where solrType = STRegex
instance SolrType TPhrase  where solrType = STPhrase
instance SolrType TFuzzed  where solrType = STFuzzed
instance SolrType TBoosted where solrType = STBoosted
instance SolrType TRange   where solrType = STRange


-- | Structures that are tagged with a Solr type.
class HasSolrType expr where
  getSolrType :: expr ty -> SSolrType ty


-- | Types that can be fuzzed by a @\'~\'@ operator.
type family FuzzableType ty :: Constraint where
  FuzzableType TWord   = SolrType TWord
  FuzzableType TPhrase = SolrType TPhrase


-- | Types that can be boosted by a @\'^\'@ operator.
type family BoostableType ty :: Constraint where
  BoostableType TWord   = SolrType TWord
  BoostableType TPhrase = SolrType TPhrase


-- | Types that can appear in a range expression.
type family PrimType ty :: Constraint where
  PrimType TNum  = SolrType TNum
  PrimType TWord = SolrType TWord

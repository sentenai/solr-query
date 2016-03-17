{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Lucene.Type where


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


class FuzzableType (a :: LuceneType) where
  type TFuzzed a :: LuceneType

instance FuzzableType TWord where
  type TFuzzed TWord = TFuzzyWord

instance FuzzableType TPhrase where
  type TFuzzed TPhrase = TFuzzyPhrase


class BoostableType (a :: LuceneType) where
  type TBoosted a :: LuceneType

instance BoostableType TWord where
  type TBoosted TWord = TBoostedWord

instance BoostableType TPhrase where
  type TBoosted TPhrase = TBoostedPhrase


class PrimType (a :: LuceneType)

instance PrimType TWord
instance PrimType TInt

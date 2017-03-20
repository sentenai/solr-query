{-# language CPP                  #-}
{-# language UndecidableInstances #-}

#if MIN_VERSION_base(4,9,0)
{-# options_ghc -fno-warn-redundant-constraints #-}
#endif

module Solr.Query.Lucene.Expr.Type where

#if MIN_VERSION_base(4,9,0)
import GHC.TypeLits (TypeError, ErrorMessage(..))
#endif

data LuceneExprTy
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
  | TSpatialPredicate

-- | 'word's and 'phrase's can fuzzed by the '~:' operator.
class Fuzzable (ty :: LuceneExprTy)
instance Fuzzable 'TWord
instance Fuzzable 'TPhrase

-- | 'word's and 'phrase's can be boosted by the '^:' operator.
class Boostable (ty :: LuceneExprTy)
instance Boostable 'TWord
instance Boostable 'TPhrase

-- | 'int's, 'float's, 'word's, and 'datetime's can 'to' range expression.
class Rangeable (a :: LuceneExprTy) (b :: LuceneExprTy)
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

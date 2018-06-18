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
  = TNum
  | TBool
  | TWord
  | TWild
  | TRegex
  | TPhrase
  | TDateTime
  | TRange
  | TSpatialPredicate

-- | 'word's and 'phrase's can be boosted by the '^:' operator.
class Boostable (ty :: LuceneExprTy)
instance Boostable 'TWord
instance Boostable 'TPhrase

-- | 'int's, 'float's, 'word's, and 'datetime's can 'to' range expression.
class Rangeable (a :: LuceneExprTy)
instance Rangeable 'TNum
instance Rangeable 'TWord
instance Rangeable 'TDateTime

#if MIN_VERSION_base(4,9,0)
instance {-# OVERLAPPABLE #-} TypeError ('Text "You can only boost words and phrases") => Boostable a
instance {-# OVERLAPPABLE #-} TypeError ('Text "You can only use numbers, words, and dates in a range expression") => Rangeable a
#endif

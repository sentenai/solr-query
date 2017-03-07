module Solr.Query.Lucene.Expr.Impl.Initial.Untyped where

import Solr.Prelude
import Solr.Query.Lucene.Expr.Class
import Solr.Query.Lucene.Expr.Type

import Data.String (IsString(..))

-- | An untyped, initially-encoded @lucene@ expression.
data UExpr (ty :: LuceneExprTy)
  = UInt Int64
  | UFloat Double
  | UTrue
  | UFalse
  | UWord Text
  | UWild Text
  | URegex Text
  | UUTCTime UTCTime
  | UDateTime TruncatedDateTime
  | forall a. UPhrase [UExpr a]
  | forall a. UFuzz (UExpr a) Int
  | forall a b. UTo (Boundary UExpr a) (Boundary UExpr b)
  | forall a. UBoost (UExpr a) Float

deriving instance Show (UExpr ty)

instance Eq (UExpr ty) where
  UInt a == UInt c = a == c
  UFloat a == UFloat c = a == c
  UTrue == UTrue = True
  UFalse == UFalse = True
  UWord a == UWord c = a == c
  UWild a == UWild c = a == c
  URegex a == URegex c = a == c
  UUTCTime a == UUTCTime c = a == c
  UDateTime a == UDateTime c = a == c
  UPhrase a == UPhrase c = coerce a == c
  UFuzz a b == UFuzz c d = coerce a == c && b == d
  UTo a b == UTo c d = eqRange a c && eqRange b d
   where
    eqRange :: Boundary UExpr a -> Boundary UExpr b -> Bool
    eqRange (Inclusive x) (Inclusive y) = coerce x == y
    eqRange (Exclusive x) (Exclusive y) = coerce x == y
    eqRange Star          Star          = True
    eqRange _             _             = False
  UBoost a b == UBoost c d = coerce a == c && b == d
  _ == _ = False

instance IsString (UExpr 'TWord) where
  fromString s = word (pack s)

instance LuceneExprSYM UExpr where
  int    = UInt
  float  = UFloat
  true   = UTrue
  false  = UFalse
  word   = UWord
  wild   = UWild
  regex  = URegex
  phrase = UPhrase
  (~:)   = UFuzz
  to     = UTo
  (^:)   = UBoost
  datetime t =
    case toDateTime t of
      UTC t'       -> UUTCTime t'
      Truncated t' -> UDateTime t'



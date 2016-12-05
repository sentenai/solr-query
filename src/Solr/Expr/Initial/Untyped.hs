module Solr.Expr.Initial.Untyped
  ( -- * Expression type
    Expr(..)
    -- * Re-exports
  , module Solr.DateTime.Internal
  , module Solr.Expr.Class
  ) where

import Solr.DateTime.Internal
import Solr.DateTime.ReallyInternal
import Solr.Expr.Class
import Solr.Type

import Data.Coerce (coerce)
import Data.Int    (Int64)
import Data.Text   (Text)
import Data.Time   (UTCTime)


-- | An untyped, initially-encoded Solr expression.
data Expr (ty :: SolrType)
  = EInt Int64
  | EFloat Double
  | ETrue
  | EFalse
  | EWord Text
  | EWild Text
  | ERegex Text
  | EUTCTime UTCTime
  | EDateTime TruncatedDateTime
  | forall b. EPhrase [Expr b]
  | forall b. EFuzz (Expr b) Int
  | forall b. ETo (Boundary (Expr b)) (Boundary (Expr b))
  | forall b. EBoost (Expr b) Float

deriving instance Show (Expr ty)

instance Eq (Expr ty) where
  EInt      a   == EInt      c   =        a == c
  EFloat    a   == EFloat    c   =        a == c
  ETrue         == ETrue         = True
  EFalse        == EFalse        = True
  EWord     a   == EWord     c   =        a == c
  EWild     a   == EWild     c   =        a == c
  ERegex    a   == ERegex    c   =        a == c
  EUTCTime  a   == EUTCTime  c   =        a == c
  EDateTime a   == EDateTime c   =        a == c
  EPhrase   a   == EPhrase   c   = coerce a == c
  EFuzz     a b == EFuzz     c d = coerce a == c &&        b == d
  ETo       a b == ETo       c d = coerce a == c && coerce b == d
  EBoost    a b == EBoost    c d = coerce a == c &&        b == d
  _             == _             = False

instance ExprSYM Expr where
  int    = EInt
  float  = EFloat
  true   = ETrue
  false  = EFalse
  word   = EWord
  wild   = EWild
  regex  = ERegex
  phrase = EPhrase
  (~:)   = EFuzz
  to     = ETo
  (^:)   = EBoost
  datetime t =
    case toDateTime t of
      UTC t'       -> EUTCTime t'
      Truncated t' -> EDateTime t'

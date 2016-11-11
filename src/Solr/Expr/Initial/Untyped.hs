module Solr.Expr.Initial.Untyped
  ( -- * Expression type
    Expr(..)
    -- * Re-exports
  , module Solr.Expr.Class
  ) where

import Solr.Expr.Class
import Solr.Type

import Data.Coerce (coerce)
import Data.Text   (Text)


-- | An untyped, initially-encoded Solr expression.
data Expr (ty :: SolrType)
  = ENum Float
  | ETrue
  | EFalse
  | EWord Text
  | EWild Text
  | ERegex Text
  | forall b. EPhrase [Expr b]
  | forall b. EFuzz (Expr b) Int
  | forall b. ETo (Boundary (Expr b)) (Boundary (Expr b))
  | forall b. EBoost (Expr b) Float

deriving instance Show (Expr ty)

instance Eq (Expr ty) where
  ENum    a   == ENum    c   =        a == c
  ETrue       == ETrue       = True
  EFalse      == EFalse      = True
  EWord   a   == EWord   c   =        a == c
  EWild   a   == EWild   c   =        a == c
  ERegex  a   == ERegex  c   =        a == c
  EPhrase a   == EPhrase c   = coerce a == c
  EFuzz   a b == EFuzz   c d = coerce a == c &&        b == d
  ETo     a b == ETo     c d = coerce a == c && coerce b == d
  EBoost  a b == EBoost  c d = coerce a == c &&        b == d
  _           == _           = False

instance ExprSYM Expr where
  num    = ENum
  true   = ETrue
  false  = EFalse
  word   = EWord
  wild   = EWild
  regex  = ERegex
  phrase = EPhrase
  (~:)   = EFuzz
  to     = ETo
  (^:)   = EBoost

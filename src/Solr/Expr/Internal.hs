module Solr.Expr.Internal where

import Builder
import Solr.Expr.Class
import Solr.Type

import Data.Semigroup (Semigroup(..))
import Data.String    (IsString(..))
import Data.Text      (pack)
import Data.Time      (defaultTimeLocale, formatTime)

-- |
-- @
-- 'Expr' :: 'SolrType' -> *
-- @
--
-- An opaque Solr expression, indexed by its 'SolrType'. Its interpretation,
-- accessed via e.g. 'Solr.Query.compile', is a lazy 'Data.Text.Lazy.Text'.
--
-- For an initially-encoded version, see "Solr.Expr.Initial.Untyped" or
-- "Solr.Expr.Initial.Typed".
newtype Expr (t :: SolrType) = E { unE :: Builder }

instance IsString (Expr 'TWord) where
  fromString s = word (pack s)

instance ExprSYM Expr where
  num n = E (bshow n)

  true = E "true"

  false = E "false"

  word s = E (thaw' s)

  wild s = E (thaw' s)

  regex s = E (char '/' <> thaw' s <> char '/')

  phrase ss = E (dquotes (spaces (map unE ss)))

  utctime t =
    E (thawStr (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" t))

  E e ~: n = E (e <> char '~' <> bshow n)

  to b1 b2 = E (lhs b1 <> " TO " <> rhs b2)
   where
    lhs :: Boundary (Expr a) -> Builder
    lhs (Inclusive e) = char '[' <> unE e
    lhs (Exclusive e) = char '{' <> unE e
    lhs Star          = "[*"

    rhs :: Boundary (Expr a) -> Builder
    rhs (Inclusive e) = unE e <> char ']'
    rhs (Exclusive e) = unE e <> char '}'
    rhs Star          = "*]"

  E e ^: n = E (e <> char '^' <> bshow n)

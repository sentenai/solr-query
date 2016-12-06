{-# language CPP #-}

module Solr.Expr.Internal where

import Builder
import Solr.DateTime.ReallyInternal
import Solr.Expr.Class
import Solr.Type

import Data.Semigroup  (Semigroup(..))
import Data.String     (IsString(..))
import Data.Text       (pack)
import Data.Time       (formatTime)
import Text.Printf     (printf)

#if MIN_VERSION_time(1,5,0)
import Data.Time      (defaultTimeLocale)
#else
import System.Locale  (defaultTimeLocale)
#endif

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
  int n = E (bshow n)

  float n = E (bshow n)

  true = E "true"

  false = E "false"

  word s = E (thaw' s)

  wild s = E (thaw' s)

  regex s = E (char '/' <> thaw' s <> char '/')

  phrase ss = E (dquotes (spaces (map unE ss)))

  datetime t =
    case toDateTime t of
      UTC t' -> E (thawStr (formatTime defaultTimeLocale "\"%Y-%m-%dT%H:%M:%S%QZ\"" t'))
      Truncated t' -> E (formatTruncated t')

  E e ~: n = E (e <> char '~' <> bshow n)

  to b1 b2 = E (lhs b1 <> " TO " <> rhs b2)
   where
    lhs :: Boundary Expr a -> Builder
    lhs (Inclusive e) = char '[' <> unE e
    lhs (Exclusive e) = char '{' <> unE e
    lhs Star          = "[*"

    rhs :: Boundary Expr a -> Builder
    rhs (Inclusive e) = unE e <> char ']'
    rhs (Exclusive e) = unE e <> char '}'
    rhs Star          = "*]"

  E e ^: n = E (e <> char '^' <> bshow n)

formatTruncated :: TruncatedDateTime -> Builder
formatTruncated =
  go '"' show
    (go '-' fmt
      (go '-' fmt
        (go 'T' fmt
          (go ':' fmt
            (go ':' fmt formatMilli)))))
  where
  go :: Char -> (a -> String) -> (b -> Builder) -> (a, Maybe b) -> Builder
  go c f g (a, b) = char c <> thawStr (f a) <> maybe (char '"') g b

  fmt :: Int -> String
  fmt = printf "%02d"

  -- Format to 5 decimal places
  formatMilli :: Millisecond -> Builder
  formatMilli ml = thawStr (tail (printf "%.5f" (ml / 100))) <> "Z\""

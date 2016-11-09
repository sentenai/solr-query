module Solr.Expr.Internal where

import Builder
import Solr.Internal.Class.Expr
import Solr.Type

import Data.Semigroup (Semigroup(..))
import Data.String    (IsString(..))
import Data.Text      (pack)


-- |
-- @
-- 'SolrExpr' :: 'SolrType' -> *
-- @
--
-- An opaque Solr expression, indexed by its 'SolrType'. Its interpretation,
-- accessed via e.g. 'Solr.Query.compileSolrQuery', is a lazy
-- 'Data.Text.Lazy.Text'.
--
-- For an initially-encoded version, see "Solr.Expr.Initial.Untyped" or
-- "Solr.Expr.Initial.Typed".
newtype SolrExpr (t :: SolrType) = Expr { unExpr :: Builder }

instance IsString (SolrExpr 'TWord) where
  fromString s = word (pack s)

instance SolrExprSYM SolrExpr where
  num n = Expr (bshow n)

  true = Expr "true"

  false = Expr "false"

  word s = Expr (thaw' s)

  wild s = Expr (thaw' s)

  regex s = Expr (char '/' <> thaw' s <> char '/')

  phrase ss = Expr (dquotes (spaces (map unExpr ss)))

  e ~: n = Expr (unExpr e <> char '~' <> bshow n)

  to b1 b2 = Expr (lhs b1 <> " TO " <> rhs b2)
   where
    lhs :: Boundary (SolrExpr a) -> Builder
    lhs (Inclusive e) = char '[' <> unExpr e
    lhs (Exclusive e) = char '{' <> unExpr e
    lhs Star          = "[*"

    rhs :: Boundary (SolrExpr a) -> Builder
    rhs (Inclusive e) = unExpr e <> char ']'
    rhs (Exclusive e) = unExpr e <> char '}'
    rhs Star          = "*]"

  e ^: n = Expr (unExpr e <> char '^' <> bshow n)

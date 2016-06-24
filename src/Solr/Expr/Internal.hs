{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Solr.Expr.Internal where

import Builder                  (Builder)
import Solr.Internal.Class.Expr
import Solr.Type

import qualified Builder

import Data.Semigroup (Semigroup(..))
import Data.String    (IsString(..))

import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text


-- | A Solr expression.
newtype SolrExpr (t :: SolrType) = Expr { unExpr :: Builder }

instance IsString (SolrExpr 'TWord) where
  fromString s = word (Text.pack s)

instance SolrExprSYM SolrExpr where
  num n = Expr (Builder.show n)

  true = Expr "true"

  false = Expr "false"

  word s = Expr (Text.encodeUtf8Builder s)

  wild s = Expr (Text.encodeUtf8Builder s)

  regex s = Expr ("/" <> Text.encodeUtf8Builder s <> "/")

  phrase ss = Expr ("\"" <> Builder.spaces (map unExpr ss) <> "\"")

  e ~: n = Expr (unExpr e <> "~" <> Builder.show n)

  to b1 b2 = Expr (lhs b1 <> " TO " <> rhs b2)
   where
    lhs :: Boundary (SolrExpr a) -> Builder
    lhs (Inclusive e) = Builder.char8 '[' <> unExpr e
    lhs (Exclusive e) = Builder.char8 '{' <> unExpr e
    lhs Star          = Builder.lazyByteString "[*"

    rhs :: Boundary (SolrExpr a) -> Builder
    rhs (Inclusive e) = unExpr e <> Builder.char8 ']'
    rhs (Exclusive e) = unExpr e <> Builder.char8 '}'
    rhs Star          = Builder.lazyByteString "*]"

  e ^: n = Expr (unExpr e <> "^" <> Builder.show n)

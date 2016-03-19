{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Solr query construction and compilation. You may prefer to import
-- "Solr.Qualified.Query" instead, which does not export any operators.

module Solr.Query
  (
  -- * Query type
    SolrQuery
  -- * Query construction
  -- $note-simplicity
  , defaultField
  , (=:)
  , (&&:)
  , (||:)
  , (-:)
  , (^=:)
  , neg
  , params
  -- * Expression type
  , SolrExpr
  -- * Expression construction
  -- $note-simplicity
  , int
  , float
  , true
  , false
  , word
  , wild
  , regex
  , phrase
  , (~:)
  , fuzzy
  , to
  , gt
  , gte
  , lt
  , lte
  , (^:)
  -- * Local parameters
  , paramDefaultField
  , paramOp
  -- * Query compilation
  , compileSolrQuery
  ) where

import Solr.Class
import Solr.Type

import Data.ByteString.Builder    (Builder)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Semigroup
import Data.String                (IsString(..))
import Data.Text                  (Text)
import GHC.Exts                   (IsList(..))

import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString.Builder    as BS
import qualified Data.ByteString.Lazy.Char8 as BS


-- | A Solr expression.
newtype SolrExpr (t :: SolrType) = Expr { unExpr :: Builder }

instance IsString (SolrExpr 'TWord) where
  fromString s = word (T.pack s)

instance IsList (SolrExpr 'TPhrase) where
  type Item (SolrExpr 'TPhrase) = SolrExpr 'TWord

  fromList = phrase
  toList = map (Expr . BS.lazyByteString) . BS.words . BS.toLazyByteString . unExpr


instance SolrExprSYM SolrExpr where
  int n = Expr (bshow n)

  float n = Expr (bshow n)

  true = Expr "true"

  false = Expr "false"

  word s = Expr (T.encodeUtf8Builder s)

  wild s = Expr (T.encodeUtf8Builder s)

  regex s = Expr ("/" <> T.encodeUtf8Builder s <> "/")

  phrase ss = Expr ("\"" <> spaces (map unExpr ss) <> "\"")

  e ~: n = Expr (unExpr e <> "~" <> bshow n)

  to b1 b2 = Expr (lhs b1 <> " TO " <> rhs b2)
   where
    lhs :: Boundary (SolrExpr a) -> Builder
    lhs (Inclusive e) = BS.char8 '[' <> unExpr e
    lhs (Exclusive e) = BS.char8 '{' <> unExpr e
    lhs Star          = BS.lazyByteString "[*"

    rhs :: Boundary (SolrExpr a) -> Builder
    rhs (Inclusive e) = unExpr e <> BS.char8 ']'
    rhs (Exclusive e) = unExpr e <> BS.char8 '}'
    rhs Star          = BS.lazyByteString "*]"

  e ^: n = Expr (unExpr e <> "^" <> bshow n)


-- | A Solr query.
--
-- The two boolean phantom types track whether or not this query has been
-- negated, and whether or not this query has local parameters.
--
-- While this approach allows fewer bad queries to typecheck, it is not
-- extensible, leaks abstraction, makes documentation more difficult to read,
-- and basically suffers from type-level boolean blindness (a \"Could not match
-- True with False\" type error is not very helpful). So, this might change
-- eventually.
data SolrQuery (isNeg :: Bool) (hasParams :: Bool) = Query { unQuery :: Builder }

-- | Appending Solr queries simply puts a space between them. To Solr, this is
-- equivalent to combining them with \'OR\'. However, this behavior can be
-- adjusted on a per-query basis using 'paramOp'.
--
-- Due to limited precedence options, ('<>') will typically require parens
-- around its arguments.
instance Semigroup (SolrQuery 'False 'False) where
  q1 <> q2 = Query (unQuery q1 <> " " <> unQuery q2)

-- | See @Semigroup@ instance.
instance Monoid (SolrQuery 'False 'False) where
  mempty = Query mempty
  mappend = (<>)

instance SolrQuerySYM SolrExpr SolrQuery where
  data ParamKey SolrQuery a where
    SolrQueryDefaultField :: ParamKey SolrQuery Text
    SolrQueryOp           :: ParamKey SolrQuery Text

  defaultField e = Query (unExpr e)

  f =: e = Query (T.encodeUtf8Builder f <> ":" <> unExpr e)

  q1 &&: q2 = Query ("(" <> unQuery q1 <> " AND " <> unQuery q2 <> ")")

  q1 ||: q2 = Query ("(" <> unQuery q1 <> " OR " <> unQuery q2 <> ")")

  q1 -: q2 = Query ("(" <> unQuery q1 <> " NOT " <> unQuery q2 <> ")")

  q ^=: n = Query ("(" <> unQuery q <> ")^=" <> bshow n)

  neg q = Query ("-" <> unQuery q)

  params ps q = Query (compileParams ps <> unQuery q)
   where
    compileParams [] = ""
    compileParams ps' = "{!" <> spaces (map compileParam ps') <> "}"

    compileParam :: Param SolrQuery -> Builder
    compileParam (Param k v) =
      case k of
        SolrQueryDefaultField -> "df=" <> T.encodeUtf8Builder v
        SolrQueryOp -> "q.op=" <> T.encodeUtf8Builder v


-- | The @\'df\'@ local parameter.
--
-- Example:
--
-- @
-- -- {!df=foo}bar
-- query :: 'SolrQuery' 'False 'True
-- query = 'params' ['paramDefaultField' '.=' "foo"] ('defaultField' ('word' "bar"))
-- @
paramDefaultField :: ParamKey SolrQuery Text
paramDefaultField = SolrQueryDefaultField

-- | The @\'op\'@ local parameter.
--
-- Stringly typed to avoid a clunky sum type like
--
-- @
-- data Val = And | Or | ...
-- @
--
-- which seems to have little value in cases like this. Instead, just pass
-- @\"AND\"@, @\"OR\"@, ...
--
-- Example:
--
-- @
-- -- {!q.op=AND}foo bar
-- query :: 'SolrQuery' 'False 'True
-- query = 'params' ['paramOp' '.=' \"AND\"] ('defaultField' ('word' "foo") '<>' 'defaultField' ('word' "bar"))
-- @
paramOp :: ParamKey SolrQuery Text
paramOp = SolrQueryOp


-- | Compile a 'SolrQuery' to a lazy 'ByteString'.
--
-- Example:
--
-- @
-- λ let ps = ['paramDefaultField' "body"]
-- λ let q = "foo" =: 'phrase' ["bar", "baz"] '~:' 5 '&&:' 'defaultField' ('regex' "wh?at")
-- λ 'compileSolrQuery' ('params' ps q)
-- "{!df=body}(foo:\\"bar baz\\"~5 AND \/wh?t\/)"
-- @
compileSolrQuery :: SolrQuery isNeg hasParams -> ByteString
compileSolrQuery = BS.toLazyByteString . unQuery


bshow :: Show a => a -> Builder
bshow = BS.lazyByteString . BS.pack . show

spaces :: [Builder] -> Builder
spaces [] = ""
spaces [w] = w
spaces (w:ws) = w <> " " <> spaces ws


-- $note-simplicity
-- For simplicity, the type signatures in the examples below monomorphise the
-- functions to use 'SolrQuery' (and therefore 'SolrExpr', due to the
-- functional dependency).

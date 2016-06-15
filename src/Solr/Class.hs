{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

-- | This module defines the finally tagless Solr DSL. This style admits
-- multiple interpreters, two of which (lazy 'Data.ByteString.Lazy.ByteString's
-- and an initial encoding) are provided by this library, in the "Solr.Query"
-- and "Solr.Query.Initial" modules, respectively.
--
-- Ordinary users should not normally have to import this module.

module Solr.Class
  (
    -- * Solr language
    SolrExprSYM(..)
  , SolrQuerySYM(..)
    -- * Derived combinators
  , fuzzy
  , gt
  , gte
  , lt
  , lte
    -- * Local parameters
  , Param(..)
  , (.=)
    -- * Range query helpers
  , Boundary(..)
  , incl
  , excl
  , star
  ) where

import Solr.Type

import Data.Text (Text)
import Prelude   hiding (all, any)

-- $setup
-- >>> import Solr.Query

-- | Solr expression.
class SolrExprSYM expr where
  -- | A @num@ expression.
  --
  -- >>> "foo" =: num 5 :: SolrQuery
  -- q=foo:5.0
  num :: Float -> expr 'TNum

  -- | A @true@ expression.
  --
  -- >>> "foo" =: true :: SolrQuery
  -- q=foo:true
  true :: expr 'TBool

  -- | A @false@ expression.
  --
  -- >>> "foo" =: false :: SolrQuery
  -- q=foo:false
  false :: expr 'TBool

  -- | A single word. Must /not/ contain any spaces, wildcard characters
  -- (@\'?\'@ and @\'*\'@), or tildes (@\'~\'@), though this is not enforced by
  -- the type system.
  --
  -- Note that sometimes you may use the 'Data.String.IsString' instance for
  -- 'Solr.Query.SolrExpr' 'TWord', but usually an explicit type signature
  -- will be required (at the interpretation site or earlier).
  --
  -- >>> "foo" =: word "bar" :: SolrQuery
  -- q=foo:bar
  word :: Text -> expr 'TWord

  -- | A single word that may contain wildcard characters (@\'?\'@ and @\'*\'@),
  -- although the meaning of consecutive @\'*\'@s is probably ill-defined. Must
  -- also /not/ contain any spaces or tildes (@\'~\'@), though this is not
  -- enforced by the type system.
  --
  -- >>> "foo" =: wild "b?r" :: SolrQuery
  -- q=foo:b?r
  wild :: Text -> expr 'TWild

  -- | A regular expression, whose syntax is described by
  -- <http://lucene.apache.org/core/5_5_0/core/org/apache/lucene/util/automaton/RegExp.html?is-external=true>.
  --
  -- Note that the leading and trailing @\'/\'@ must be omitted. The regex
  -- innards are not type checked in any way.
  --
  -- >>> "foo" =: regex "[mb]oat" :: SolrQuery
  -- q=foo:/[mb]oat/
  regex :: Text -> expr 'TRegex

  -- | A phrase, composed of multiple (non-fuzzy) words, none of which may
  -- contain wildcard characters. Both of these properties are enforced by the
  -- type system, as long as the words themselves adhere to the 'word' contract.
  -- The list should not be empty.
  --
  -- >>> "foo" =: phrase ["bar", "baz"] :: SolrQuery
  -- q=foo:"bar baz"
  phrase :: [expr 'TWord] -> expr 'TPhrase

  -- | The @\'~\'@ operator, which fuzzes its argument (either a word or phrase)
  -- by a numeric amount.
  --
  -- >>> "foo" =: word "bar" ~: 1 :: SolrQuery
  -- q=foo:bar~1
  --
  -- >>> "foo" =: phrase ["bar", "baz", "qux"] ~: 10 :: SolrQuery
  -- q=foo:"bar baz qux"~10
  (~:) :: FuzzableType a => expr a -> Int -> expr 'TFuzzed
  infix 6 ~:

  -- | A range expression.
  --
  -- >>> "foo" =: incl (num 5) `to` excl (num 10) :: SolrQuery
  -- q=foo:[5.0 TO 10.0}
  --
  -- >>> "foo" =: excl (word "bar") `to` star :: SolrQuery
  -- q=foo:{bar TO *]
  --
  -- -- Note the explicit type signature required for @[* TO *]@ queries
  -- >>> "foo" =: star `to` (star :: Boundary (SolrExpr 'TNum)) :: SolrQuery
  -- q=foo:[* TO *]
  to :: PrimType a => Boundary (expr a) -> Boundary (expr a) -> expr 'TRange
  infix 6 `to`

  -- | The @\'^\'@ operator, which boosts its argument.
  --
  -- >>> "foo" =: word "bar" ^: 3.5 :: SolrQuery
  -- q=foo:bar^3.5
  --
  -- >>> "foo" =: phrase ["bar", "baz"] ^: 3.5 :: SolrQuery
  -- q=foo:"bar baz"^3.5
  (^:) :: BoostableType a => expr a -> Float -> expr 'TBoosted
  infix 6 ^:


-- | Short-hand for fuzzing a word by 2. This is the default behavior of a
-- Solr @\'~\'@ operator without an integer added.
--
-- @
-- 'fuzzy' e = e '~:' 2
-- @
--
-- >>> "foo" =: fuzzy "bar" :: SolrQuery
-- q=foo:bar~2
fuzzy :: SolrExprSYM expr => expr 'TWord -> expr 'TFuzzed
fuzzy e = e ~: 2

-- | Short-hand for a greater-than range query.
--
-- @
-- 'gt' e = 'excl' e \`to\` 'star'
-- @
--
-- >>> "foo" =: gt (num 5) :: SolrQuery
-- q=foo:{5.0 TO *]
gt :: (SolrExprSYM expr, PrimType a) => expr a -> expr 'TRange
gt e = excl e `to` star

-- | Short-hand for a greater-than-or-equal-to range query.
--
-- @
-- 'gte' e = 'incl' e \`to\` 'star'
-- @
--
-- >>> "foo" =: gte (num 5) :: SolrQuery
-- q=foo:[5.0 TO *]
gte :: (SolrExprSYM expr, PrimType a) => expr a -> expr 'TRange
gte e = incl e `to` star

-- | Short-hand for a less-than range query.
--
-- @
--  'lt' e = 'star' \`to\` 'excl' e
-- @
--
-- >>> "foo" =: lt (num 5) :: SolrQuery
-- q=foo:[* TO 5.0}
lt :: (SolrExprSYM expr, PrimType a) => expr a -> expr 'TRange
lt e = star `to` excl e

-- | Short-hand for a less-than-or-equal-to range query.
--
-- @
-- 'lte' e = 'star' \`to\` 'incl' e
-- @
--
-- >>> "foo" =: lte (num 5) :: SolrQuery
-- q=foo:[* TO 5.0]
lte :: (SolrExprSYM expr, PrimType a) => expr a -> expr 'TRange
lte e = star `to` incl e


-- | Solr query.
class SolrExprSYM expr => SolrQuerySYM expr query | query -> expr where
  -- | Different queries support different sets of local parameters. Each
  -- parameter is indexed by the type of its value.
  --
  -- Example:
  --
  -- @
  -- data ParamKey MyQuery a where
  --   Foo :: ParamKey MyQuery Int
  --   Bar :: ParamKey MyQuery Bool
  --   Baz :: ParamKey MyQuery String
  -- @
  data ParamKey query :: * -> *

  -- | A default field query.
  --
  -- >>> defaultField (word "foo") :: SolrQuery
  -- q=foo
  defaultField :: expr a -> query

  -- | A field query.
  --
  -- >>> "foo" =: word "bar" :: SolrQuery
  -- q=foo:bar
  (=:) :: Text -> expr a -> query
  infix 5 =:

  -- | An @AND@ query.
  --
  -- >>> "foo" =: word "bar" &&: "baz" =: word "qux" :: SolrQuery
  -- q=(foo:bar AND baz:qux)
  (&&:) :: query -> query -> query
  infixr 3 &&:

  -- | An @OR@ query.
  --
  -- >>> "foo" =: word "bar" ||: "baz" =: word "qux" :: SolrQuery
  -- q=(foo:bar OR baz:qux)
  (||:) :: query -> query -> query
  infixr 2 ||:

  -- | A @NOT@, @\'!\'@, or @\'-\'@ query.
  --
  -- >>> "foo" =: word "bar" -: "baz" =: word "qux" :: SolrQuery
  -- q=(foo:bar NOT baz:qux)
  (-:) :: query -> query -> query
  infixr 1 -:

  -- | The @\'^=\'@ constant score operator.
  --
  -- This is given right-fixity to reject queries like @q ^= 1 ^= 2@.
  --
  -- >>> "foo" =: word "bar" ^=: 3.5 :: SolrQuery
  -- q=foo:bar^=3.5
  (^=:) :: query -> Float -> query
  infixr 4 ^=:

  -- | Negate a query.
  --
  -- >>> neg ("foo" =: word "bar") :: SolrQuery
  -- q=-foo:bar
  neg :: query -> query

  -- | Add local parameters to a query.
  --
  -- >>> params [paramDefaultField .= "foo"] (defaultField (word "bar")) :: SolrQuery
  -- q={!df=foo}bar
  params :: [Param query] -> query -> query


-- | A parameter is built from a key and a value, whose type depends on the key.
data Param query where
  Param :: ParamKey query val -> val -> Param query

-- | Infix constructor for 'Param'.
(.=) :: ParamKey query val -> val -> Param query
(.=) = Param


-- | An inclusive or exclusive expression for use in a range query, built with
-- either 'incl', 'excl', or 'star'.
--
-- The constructors are exported for use in interpreters.
data Boundary a
  = Inclusive a
  | Exclusive a
  | Star

-- | Mark an expression as inclusive, for use in a range query.
incl :: SolrExprSYM expr => expr a -> Boundary (expr a)
incl = Inclusive

-- | Mark an expression as exclusive, for use in a range query.
excl :: SolrExprSYM expr => expr a -> Boundary (expr a)
excl = Exclusive

-- | @\'*\'@ operator, signifying the minimum or maximun bound of a range. A
-- @[* TO *]@ query will require a type annotation.
star :: SolrExprSYM expr => Boundary (expr a)
star = Star

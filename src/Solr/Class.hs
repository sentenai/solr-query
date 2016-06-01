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
  , qall
  , qany
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

import Data.Monoid (Monoid(..))
import Data.Text   (Text)
import Prelude     hiding (all, any)

-- | Solr expression.
class SolrExprSYM expr where
  -- | A @num@ expression.
  --
  -- Example:
  --
  -- @
  -- -- foo:5
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'num' 5
  -- @
  num :: Float -> expr 'TNum

  -- | A @true@ expression.
  --
  -- Example:
  --
  -- @
  -- -- foo:true
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'true'
  -- @
  true :: expr 'TBool

  -- | A @false@ expression.
  --
  -- Example:
  --
  -- @
  -- -- foo:false
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'false'
  -- @
  false :: expr 'TBool

  -- | A single word. Must /not/ contain any spaces, wildcard characters
  -- (@\'?\'@ and @\'*\'@), or tildes (@\'~\'@), though this is not enforced by
  -- the type system.
  --
  -- Note that sometimes you may use the 'Data.String.IsString' instance for
  -- 'Solr.Query.SolrExpr' 'TWord', but usually an explicit type signature
  -- will be required (at the interpretation site or earlier).
  --
  -- Example:
  --
  -- @
  -- -- foo:bar
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'word' "bar"
  --
  -- -- foo:bar
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' ("bar" :: 'Solr.Query.SolrExpr' 'TWord')
  -- @
  word :: Text -> expr 'TWord

  -- | A single word that may contain wildcard characters (@\'?\'@ and @\'*\'@),
  -- although the meaning of consecutive @\'*\'@s is probably ill-defined. Must
  -- also /not/ contain any spaces or tildes (@\'~\'@), though this is not
  -- enforced by the type system.
  --
  -- Example:
  --
  -- @
  -- -- foo:b?r
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'wild' "b?r"
  -- @
  wild :: Text -> expr 'TWild

  -- | A regular expression, whose syntax is described by
  -- <http://lucene.apache.org/core/5_5_0/core/org/apache/lucene/util/automaton/RegExp.html?is-external=true>.
  --
  -- Note that the leading and trailing @\'/\'@ must be omitted. The regex
  -- innards are not type checked in any way.
  --
  -- @
  -- -- foo:\/[mb]oat\/
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'regex' "[mb]oat"
  -- @
  regex :: Text -> expr 'TRegex

  -- | A phrase, composed of multiple (non-fuzzy) words, none of which may
  -- contain wildcard characters. Both of these properties are enforced by the
  -- type system, as long as the words themselves adhere to the 'word' contract.
  -- The list should not be empty.
  --
  -- Note that sometimes you may use the 'GHC.Exts.IsList' instance for
  -- 'Solr.Query.SolrExpr' 'TPhrase', but usually an explicit type signature
  -- will be required (at the interpretation site or earlier).
  --
  -- Example:
  --
  -- @
  -- -- foo:"bar baz"
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'phrase' ["bar", "baz"] -- ok
  --
  -- -- foo:"bar b?z" (an invalid Solr query)
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'phrase' ["bar", 'wild' "b?z"] -- type error
  --
  -- -- foo:"bar b?z" (an invalid Solr query)
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'phrase' ["bar", "b?z"] -- breaks 'word' contract
  -- @
  --
  -- Or, with @OverloadedLists@:
  --
  -- @
  -- -- foo:"bar baz"
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' (["bar", "baz"] :: 'Solr.Query.SolrExpr' 'TPhrase')
  -- @
  phrase :: [expr 'TWord] -> expr 'TPhrase

  -- | The @\'~\'@ operator, which fuzzes its argument (either a word or phrase)
  -- by a numeric amount.
  --
  -- Example:
  --
  -- @
  -- -- foo:bar~1
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'word' "bar" '~:' 1
  --
  -- -- foo:"bar baz qux"~10
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'phrase' ["bar", "baz", "qux"] '~:' 10
  -- @
  (~:) :: FuzzableType a => expr a -> Int -> expr 'TFuzzed
  infix 6 ~:

  -- | A range expression.
  --
  -- Example:
  --
  -- @
  -- -- foo:[5 TO 10}
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'incl' ('num' 5) \`to\` 'excl' ('num' 10)
  -- @
  to :: PrimType a => Boundary (expr a) -> Boundary (expr a) -> expr 'TRange
  infix 6 `to`

  -- | The @\'^\'@ operator, which boosts its argument.
  --
  -- Example:
  --
  -- @
  -- -- foo:bar^3.5
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'word' "bar" '^:' 3.5
  --
  -- -- foo:"bar baz"^3.5
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'phrase' ["bar", "baz"] '^:' 3.5
  -- @
  (^:) :: BoostableType a => expr a -> Float -> expr 'TBoosted
  infix 6 ^:


-- | Short-hand for fuzzing a word by 2. This is the default behavior of a
-- Solr @\'~\'@ operator without an integer added.
--
-- @
-- 'fuzzy' e = e '~:' 2
-- @
--
-- Example:
--
-- @
-- -- foo:bar~
-- query :: 'Solr.Query.SolrQuery'
-- query = "foo" '=:' 'fuzzy' "bar"
-- @
fuzzy :: SolrExprSYM expr => expr 'TWord -> expr 'TFuzzed
fuzzy e = e ~: 2

-- | Short-hand for a greater-than range query.
--
-- @
-- 'gt' e = 'excl' e \`to\` 'star'
-- @
--
-- Example:
--
-- @
-- -- foo:>5
-- -- foo:{5 TO *]
-- query :: 'Solr.Query.SolrQuery'
-- query = "foo" '=:' 'gt' ('num' 5)
-- @
gt :: (SolrExprSYM expr, PrimType a) => expr a -> expr 'TRange
gt e = excl e `to` star

-- | Short-hand for a greater-than-or-equal-to range query.
--
-- @
-- 'gte' e = 'incl' e \`to\` 'star'
-- @
--
-- Example:
--
-- @
-- -- foo:>=5
-- -- foo:[5 TO *]
-- query :: 'Solr.Query.SolrQuery'
-- query = "foo" '=:' 'gte' ('num' 5)
-- @
gte :: (SolrExprSYM expr, PrimType a) => expr a -> expr 'TRange
gte e = incl e `to` star

-- | Short-hand for a less-than range query.
--
-- @
--  'lt' e = 'star' \`to\` 'excl' e
-- @
--
-- Example:
--
-- @
-- -- foo:<5
-- -- foo:[* TO 5}
-- query :: 'Solr.Query.SolrQuery'
-- query = "foo" '=:' 'lt' ('num' 5)
-- @
lt :: (SolrExprSYM expr, PrimType a) => expr a -> expr 'TRange
lt e = star `to` excl e

-- | Short-hand for a less-than-or-equal-to range query.
--
-- @
-- 'lte' e = 'star' \`to\` 'incl' e
-- @
--
-- Example:
--
-- @
-- -- foo:<=5
-- -- foo:[* TO 5]
-- query :: 'Solr.Query.SolrQuery'
-- query = "foo" '=:' 'lte' ('num' 5)
-- @
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
  -- Example:
  --
  -- @
  -- -- foo
  -- query :: 'Solr.Query.SolrQuery'
  -- query = 'defaultField' ('word' "foo")
  -- @
  defaultField :: expr a -> query

  -- | A field query.
  --
  -- Example:
  --
  -- @
  -- -- foo:bar
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'word' "bar"
  -- @
  (=:) :: Text -> expr a -> query
  infix 5 =:

  -- | An @AND@ query.
  --
  -- Example:
  --
  -- @
  -- -- foo:bar AND baz:qux
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'word' "bar"
  --     '&&:' "baz" '=:' 'word' "qux"
  -- @
  (&&:) :: query -> query -> query
  infixr 3 &&:

  -- | An @OR@ query.
  --
  -- Example:
  --
  -- @
  -- -- foo:bar OR baz:qux
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'word' "bar"
  --     '||:' "baz" '=:' 'word' "qux"
  -- @
  (||:) :: query -> query -> query
  infixr 2 ||:

  -- | A @NOT@, @\'!\'@, or @\'-\'@ query.
  --
  -- Example:
  --
  -- @
  -- -- foo:bar NOT baz:qux
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'word' "bar"
  --      '-:' "baz" '=:' 'word' "qux"
  -- @
  (-:) :: query -> query -> query
  infixr 1 -:

  -- | The @\'^=\'@ constant score operator.
  --
  -- This is given right-fixity to reject queries like @q ^= 1 ^= 2@, which may
  -- very well be a valid Solr query (I haven't tested), but are nonetheless
  -- nonsense.
  --
  -- Example:
  --
  -- @
  -- -- (foo:bar)^=3.5
  -- query :: 'Solr.Query.SolrQuery'
  -- query = "foo" '=:' 'word' "bar" '^=:' 3.5
  -- @
  (^=:) :: query -> Float -> query
  infixr 4 ^=:

  -- | Negate a query.
  --
  -- Example:
  --
  -- @
  -- -- -foo:bar
  -- query :: 'Solr.Query.SolrQuery'
  -- query = 'neg' ("foo" '=:' 'word' "bar")
  -- @
  neg :: query -> query

  -- | Add local parameters to a query.
  --
  -- Example:
  --
  -- @
  -- -- {!df=foo}bar
  -- query :: 'Solr.Query.SolrQuery'
  -- query = 'params' ['Solr.Query.SolrQuery.paramDefaultField' '.=' "foo"] ('defaultField' ('word' "bar"))
  -- @
  params :: [Param query] -> query -> query

-- | Fold a list of queries with ('&&:').
--
-- @
-- qall [q1,q2,q3] = q1 '&&:' q2 '&&:' q3
-- @
qall :: (Monoid query, SolrQuerySYM expr query) => [query] -> query
qall []  = mempty
qall [q] = q
qall (q:qs) = q &&: qall qs

-- | Fold a list of queries with ('||:').
--
-- @
-- 'qall' [q1,q2,q3] = q1 '||:' q2 '||:' q3
-- @
qany :: (Monoid query, SolrQuerySYM expr query) => [query] -> query
qany []  = mempty
qany [q] = q
qany (q:qs) = q ||: qall qs


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

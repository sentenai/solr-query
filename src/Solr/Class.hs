{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

-- | This module defines the finally tagless Solr DSL. This style admits
-- multiple interpreters, one of which (lazy 'Data.ByteString.Lazy.ByteString's)
-- is provided by this library in the "Solr.Query" module.
--
-- Users should instead import either "Solr.Query" or "Solr.Qualified.Query".

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
    -- * Range query helpers
  , Boundary(..)
  , incl
  , excl
  , star
  ) where

import Solr.Type

import Data.Text (Text)

-- | Solr expression.
class SolrExprSYM (expr :: SolrType -> *) where
  -- | An @int@ expression.
  --
  -- Note that sometimes you may use the 'Num' instance for
  -- 'Solr.Query.SolrExpr' 'TInt', but usually an explicit type signature
  -- will be required (at the interpretation site or earlier).
  --
  -- Example:
  --
  -- @
  -- -- foo:5
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'int' 5
  -- @
  int :: Int -> expr 'TInt

  -- | A @true@ expression.
  --
  -- Example:
  --
  -- @
  -- -- foo:true
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'true'
  -- @
  true :: expr 'TBool

  -- | A @false@ expression.
  --
  -- Example:
  --
  -- @
  -- -- foo:false
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
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
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'word' "bar"
  --
  -- -- foo:bar
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
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
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
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
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
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
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'phrase' ["bar", "baz"] -- ok
  --
  -- -- foo:"bar b?z" (an invalid Solr query)
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'phrase' ["bar", 'wild' "b?z"] -- type error
  --
  -- -- foo:"bar b?z" (an invalid Solr query)
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'phrase' ["bar", "b?z"] -- breaks 'word' contract
  -- @
  --
  -- Or, with @OverloadedLists@:
  --
  -- @
  -- -- foo:"bar baz"
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' (["bar", "baz"] :: 'Solr.Query.SolrExpr' 'TPhrase')
  -- @
  phrase :: [expr 'TWord] -> expr 'TPhrase

  -- | The @\'~\'@ operator, which fuzzes its argument (either a word or phrase)
  -- by a numeric amount.
  --
  -- This will have one of the following two types:
  --
  -- @
  -- (~:) :: 'Solr.Query.SolrExpr' 'TWord'   -> Int -> 'Solr.Query.SolrExpr' 'TFuzzyWord'   -- Int must be 0, 1, or 2
  -- (~:) :: 'Solr.Query.SolrExpr' 'TPhrase' -> Int -> 'Solr.Query.SolrExpr' 'TFuzzyPhrase' -- Int must be positive
  -- @
  --
  -- Example:
  --
  -- @
  -- -- foo:bar~1
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'word' "bar" '~:' 1
  --
  -- -- foo:"bar baz qux"~10
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'phrase' ["bar", "baz", "qux"] '~:' 10
  -- @
  (~:) :: FuzzableType a => expr a -> Int -> expr (TFuzzed a)
  infix 6 ~:

  -- | A range expression.
  --
  -- This will have one of the following two types:
  --
  -- @
  -- to :: 'Boundary' ('Solr.Query.SolrExpr' 'TWord') -> 'Boundary' ('Solr.Query.SolrExpr' 'TWord') -> 'Solr.Query.SolrExpr' 'TRange'
  -- to :: 'Boundary' ('Solr.Query.SolrExpr' 'TInt')  -> 'Boundary' ('Solr.Query.SolrExpr' 'TInt')  -> 'Solr.Query.SolrExpr' 'TRange'
  -- @
  --
  -- Example:
  --
  -- @
  -- -- foo:[5 TO 10}
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'incl' ('int' 5) \`to\` 'excl' ('int' 10)
  -- @
  to :: PrimType a => Boundary (expr a) -> Boundary (expr a) -> expr 'TRange
  infix 6 `to`

  -- | The @\'^\'@ operator, which boosts its argument.
  --
  -- This will have one of the following two types:
  --
  -- @
  -- (^:) :: 'Solr.Query.SolrExpr' 'TWord'   -> Float -> 'Solr.Query.SolrExpr' 'TBoostedWord'
  -- (^:) :: 'Solr.Query.SolrExpr' 'TPhrase' -> Float -> 'Solr.Query.SolrExpr' 'TBoostedPhrase'
  -- @
  --
  -- Example:
  --
  -- @
  -- -- foo:bar^3.5
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'word' "bar" '^:' 3.5
  --
  -- -- foo:"bar baz"^3.5
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'phrase' ["bar", "baz"] '^:' 3.5
  -- @
  (^:) :: BoostableType a => expr a -> Float -> expr (TBoosted a)
  infix 6 ^:


-- | Short-hand for fuzzing a word by 2. This is the default behavior of a
-- Solr @\'~\'@ operator without an integer added.
--
-- @
-- 'fuzzy' e = 'fuzz' e 2
-- @
--
-- Example:
--
-- @
-- -- foo:bar~
-- query :: 'Solr.Query.SolrQuery' 'False 'False
-- query = "foo" '=:' 'fuzzy' "bar"
-- @
fuzzy :: SolrExprSYM expr => expr 'TWord -> expr 'TFuzzyWord
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
-- query :: 'Solr.Query.SolrQuery' 'False 'False
-- query = "foo" '=:' 'gt' ('int' 5)
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
-- query :: 'Solr.Query.SolrQuery' 'False 'False
-- query = "foo" '=:' 'gte' ('int' 5)
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
-- query :: 'Solr.Query.SolrQuery' 'False 'False
-- query = "foo" '=:' 'lt' ('int' 5)
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
-- query :: 'Solr.Query.SolrQuery' 'False 'False
-- query = "foo" '=:' 'lte' ('int' 5)
-- @
lte :: (SolrExprSYM expr, PrimType a) => expr a -> expr 'TRange
lte e = star `to` incl e


-- | Solr query.
class SolrExprSYM expr => SolrQuerySYM expr query | query -> expr where
  data LocalParams query :: *

  -- | A default field query.
  --
  -- Example:
  --
  -- @
  -- -- foo
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = 'defaultField' ('word' "foo")
  -- @
  defaultField :: expr a -> query 'False 'False

  -- | A field query.
  --
  -- Example:
  --
  -- @
  -- -- foo:bar
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'word' "bar"
  -- @
  (=:) :: Text -> expr a -> query 'False 'False
  infix 5 =:

  -- | An @AND@ query.
  --
  -- Example:
  --
  -- @
  -- -- foo:bar AND baz:qux
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'word' "bar"
  --     '&&:' "baz" '=:' 'word' "qux"
  -- @
  (&&:) :: query 'False 'False -> query 'False 'False -> query 'False 'False
  infixr 3 &&:

  -- | An @OR@ query.
  --
  -- Example:
  --
  -- @
  -- -- foo:bar OR baz:qux
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'word' "bar"
  --     '||:' "baz" '=:' 'word' "qux"
  -- @
  (||:) :: query 'False 'False -> query 'False 'False -> query 'False 'False
  infixr 2 ||:

  -- | A @NOT@, @\'!\'@, or @\'-\'@ query.
  --
  -- Example:
  --
  -- @
  -- -- foo:bar NOT baz:qux
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'word' "bar"
  --      '-:' "baz" '=:' 'word' "qux"
  -- @
  (-:) :: query 'False 'False -> query 'False 'False -> query 'False 'False
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
  -- query :: 'Solr.Query.SolrQuery' 'False 'False
  -- query = "foo" '=:' 'word' "bar" '^=:' 3.5
  -- @
  (^=:) :: query 'False 'False -> Float -> query 'False 'False
  infixr 4 ^=:

  -- | Negate a top-level query.
  --
  -- Only a top level query can be negated up to one time, hence the boolean tag
  -- that tracks whether or not a query has been negated.
  --
  -- Example:
  --
  -- @
  -- -- -(foo:bar)
  -- query :: 'Solr.Query.SolrQuery' 'True 'False
  -- query = 'neg' ("foo" '=:' 'word' "bar")
  -- @
  neg :: query 'False 'False -> query 'True 'False

  -- | Add local parameters to a query.
  --
  -- Only a top level query can have up to one set of local parameters, hence the
  -- boolean tag that tracks whether or not they've been added.
  --
  -- Example:
  --
  -- @
  -- -- {!df=foo}bar
  -- query :: 'Solr.Query.SolrQuery' 'False 'True
  -- query = 'localParams' ('Solr.Query.SolrQuery.paramDefaultField' "foo") ('defaultField' ('word' "bar"))
  -- @
  localParams :: LocalParams query -> query a 'False -> query a 'True



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
-- @[* TO *]@ query is allowed, but will require a type annotation.
star :: SolrExprSYM expr => Boundary (expr a)
star = Star

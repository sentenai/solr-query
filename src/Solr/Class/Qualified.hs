{-# LANGUAGE DataKinds #-}

-- | This module is an alternative to "Solr.Class" that does not export any
-- operators, and is intended to be imported qualified, because it contains
-- function names that clash with the Prelude.
--
-- > import qualified Solr.Class.Qualified as Solr
--
-- Here is a quick conversion guide:
--
-- @
-- ('Solr.Class.~:')  = 'fuzz'
-- ('Solr.Class.^:')  = 'boost'
-- ('Solr.Class.=:')  = 'field'
-- ('Solr.Class.&&:') = 'and'
-- ('Solr.Class.||:') = 'or'
-- ('Solr.Class.-:')  = 'not'
-- ('Solr.Class.^=:') = 'score'
-- @

module Solr.Class.Qualified
  (
    -- * Solr language
    SolrExprSYM
  , int
  , true
  , false
  , word
  , wild
  , regex
  , phrase
  , fuzz
  , to
  , boost
  , SolrQuerySYM
  , defaultField
  , field
  , and
  , or
  , not
  , score
  , localParams
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
import Solr.Class (Boundary(..), LocalParams, SolrExprSYM, SolrQuerySYM, excl, incl, star)

import qualified Solr.Class as Solr

import Data.Text (Text)
import Prelude   hiding (and, not, or)

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
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'int' 5)
-- @
int :: SolrExprSYM expr => Int -> expr 'TInt
int = Solr.int

-- | A @true@ expression.
--
-- Example:
--
-- @
-- -- foo:true
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" Solr.'true'
-- @
true :: SolrExprSYM expr => expr 'TBool
true = Solr.true

-- | A @false@ expression.
--
-- Example:
--
-- @
-- -- foo:false
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" Solr.'false'
-- @
false :: SolrExprSYM expr => expr 'TBool
false = Solr.false

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
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'word' "bar")
--
-- -- foo:bar
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" ("bar" :: Solr.'Solr.Query.SolrExpr' 'TWord')
-- @
word :: SolrExprSYM expr => Text -> expr 'TWord
word = Solr.word

-- | A single word that may contain wildcard characters (@\'?\'@ and @\'*\'@),
-- although the meaning of consecutive @\'*\'@s is probably ill-defined. Must
-- also /not/ contain any spaces or tildes (@\'~\'@), though this is not
-- enforced by the type system.
--
-- Example:
--
-- @
-- -- foo:b?r
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'wild' "b?r")
-- @
wild :: SolrExprSYM expr => Text -> expr 'TWild
wild = Solr.wild

-- | A regular expression, whose syntax is described by
-- <http://lucene.apache.org/core/5_5_0/core/org/apache/lucene/util/automaton/RegExp.html?is-external=true>.
--
-- Note that the leading and trailing @\'/\'@ must be omitted. The regex
-- innards are not type checked in any way.
--
-- @
-- -- foo:\/[mb]oat\/
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'regex' "[mb]oat")
-- @
regex :: SolrExprSYM expr => Text -> expr 'TRegex
regex = Solr.regex

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
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'phrase' ["bar", "baz"]) -- ok
--
-- -- foo:"bar b?z" (an invalid Solr query)
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'phrase' ["bar", Solr.'wild' "b?z"]) -- type error
--
-- -- foo:"bar b?z" (an invalid Solr query)
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'phrase' ["bar", "b?z"]) -- breaks 'word' contract
-- @
--
-- Or, with @OverloadedLists@:
--
-- @
-- -- foo:"bar baz"
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (["bar", "baz"] :: Solr.'Solr.Query.SolrExpr' 'TPhrase')
-- @
phrase :: SolrExprSYM expr => [expr 'TWord] -> expr 'TPhrase
phrase = Solr.phrase

-- | The @\'~\'@ operator, which fuzzes its argument (either a word or phrase)
-- by a numeric amount.
--
-- This will have one of the following two types:
--
-- @
-- fuzz :: 'Solr.Query.SolrExpr' 'TWord'   -> Int -> 'Solr.Query.SolrExpr' 'TFuzzyWord'   -- Int must be 0, 1, or 2
-- fuzz :: 'Solr.Query.SolrExpr' 'TPhrase' -> Int -> 'Solr.Query.SolrExpr' 'TFuzzyPhrase' -- Int must be positive
-- @
--
-- Example:
--
-- @
-- -- foo:bar~1
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'fuzz' (Solr.'word' "bar") 1)
--
-- -- foo:"bar baz qux"~10
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'fuzz' (Solr.'phrase' ["bar", "baz", "qux"]) 10)
-- @
fuzz :: (SolrExprSYM expr, FuzzableType a) => expr a -> Int -> expr (TFuzzed a)
fuzz = (Solr.~:)

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
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'incl' (Solr.'int' 5) \`Solr.to\` Solr.'excl' (Solr.'int' 10))
-- @
to :: (SolrExprSYM expr, PrimType a) => Boundary (expr a) -> Boundary (expr a) -> expr 'TRange
to = Solr.to
infix 6 `to`

-- | The @\'^\'@ operator, which boosts its argument.
--
-- This will have one of the following two types:
--
-- @
-- boost :: 'Solr.Query.SolrExpr' 'TWord'   -> Float -> 'Solr.Query.SolrExpr' 'TBoostedWord'
-- boost :: 'Solr.Query.SolrExpr' 'TPhrase' -> Float -> 'Solr.Query.SolrExpr' 'TBoostedPhrase'
-- @
--
-- Example:
--
-- @
-- -- foo:bar^3.5
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'boost' (Solr.'word' "bar") 3.5)
--
-- -- foo:"bar baz"^3.5
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'boost' (Solr.'phrase' ["bar", "baz"]) 3.5)
-- @
boost :: (SolrExprSYM expr, BoostableType a) => expr a -> Float -> expr (TBoosted a)
boost = (Solr.^:)

-- | A default field query.
--
-- Example:
--
-- @
-- -- foo
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'defaultField' (Solr.'word' "foo")
-- @
defaultField :: SolrQuerySYM expr query => expr a -> query 'False
defaultField = Solr.defaultField

-- | A field query.
--
-- Example:
--
-- @
-- -- foo:bar
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'word' "bar")
-- @
field :: SolrQuerySYM expr query => Text -> expr a -> query 'False
field = (Solr.=:)

-- | An @AND@ query.
--
-- Example:
--
-- @
-- -- foo:bar AND baz:qux
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'word' "bar") \`Solr.and\` Solr.'field' "baz" (Solr.'word' "qux")
-- @
and :: SolrQuerySYM expr query => query 'False -> query 'False -> query 'False
and = (Solr.&&:)
infixr 3 `and`

-- | An @OR@ query.
--
-- Example:
--
-- @
-- -- foo:bar OR baz:qux
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'word' "bar") \`Solr.or\` Solr.'field' "baz" (Solr.'word' "qux")
-- @
or :: SolrQuerySYM expr query => query 'False -> query 'False -> query 'False
or = (Solr.||:)
infixr 2 `or`

-- | A @NOT@, @\'!\'@ or @\'-\'@ query.
--
-- Example:
--
-- @
-- -- foo:bar NOT baz:qux
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'word' "bar") \`Solr.not\` Solr.'field' "baz" (Solr.'word' "qux")
-- @
not :: SolrQuerySYM expr query => query 'False -> query 'False -> query 'False
not = (Solr.-:)
infixr 1 `not`

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
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'score' (Solr.'field' "foo" (Solr.'word' "bar")) 3.5
-- @
score :: SolrQuerySYM expr query => query 'False -> Float -> query 'False
score = (Solr.^=:)
infixr 4 `score`

-- | Add local parameters to a query. A query can only have one set of local
-- parameters, hence the boolean tag that tracks whether or not they've been
-- added.
--
-- Example:
--
-- @
-- -- {!df=foo}bar
-- query :: Solr.'Solr.Query.SolrQuery' 'True
-- query = Solr.'localParams' (Solr.'Solr.Query.SolrQuery.paramDefaultField' "foo") (Solr.'defaultField' (Solr.'word' "bar"))
-- @
localParams :: SolrQuerySYM expr query => LocalParams query -> query 'False -> query 'True
localParams = Solr.localParams


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
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'fuzzy' "bar")
-- @
fuzzy :: SolrExprSYM expr => expr 'TWord -> expr 'TFuzzyWord
fuzzy = Solr.fuzzy

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
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'gt' (Solr.'int' 5))
-- @
gt :: (SolrExprSYM expr, PrimType a) => expr a -> expr 'TRange
gt = Solr.gt

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
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'gte' (Solr.'int' 5))
-- @
gte :: (SolrExprSYM expr, PrimType a) => expr a -> expr 'TRange
gte = Solr.gte

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
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'lt' (Solr.'int' 5))
-- @
lt :: (SolrExprSYM expr, PrimType a) => expr a -> expr 'TRange
lt = Solr.lt

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
-- query :: Solr.'Solr.Query.SolrQuery' 'False
-- query = Solr.'field' "foo" (Solr.'lte' (Solr.'int' 5))
-- @
lte :: (SolrExprSYM expr, PrimType a) => expr a -> expr 'TRange
lte = Solr.lte

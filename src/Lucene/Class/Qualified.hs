{-# LANGUAGE DataKinds #-}

-- | This module is an alternative to "Lucene.Class" that does not export any
-- operators, and is intended to be imported qualified, because it contains
-- function names that clash with the Prelude.
--
-- > import qualified Lucene.Class.Qualified as Lucene
--
-- Here is a quick conversion guide:
--
-- @
-- ('~:')  = 'fuzz'
-- ('^:')  = 'boost'
-- ('=:')  = 'field'
-- ('&&:') = 'and'
-- ('||:') = 'or'
-- ('-:')  = 'not'
-- ('^=:') = 'score'
-- @

module Lucene.Class.Qualified
  (
    -- * Lucene language
    Lucene()
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
  , field
  , and
  , or
  , not
  , score
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

import Lucene.Type
import Lucene.Class (Boundary(..), Lucene, excl, incl, star)

import qualified Lucene.Class as L

import Data.Text (Text)
import Prelude   hiding (and, not, or)

-- | An @int@ expression.
--
-- Note that sometimes you may use the 'Num' instance for
-- 'Lucene.Query.LuceneExpr' 'TInt', but usually an explicit type signature
-- will be required (at the interpretation site or earlier).
--
-- Example:
--
-- @
-- -- foo:5
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'int' 5)
-- @
int :: Lucene expr query => Int -> expr 'TInt
int = L.int

-- | A @true@ expression.
--
-- Example:
--
-- @
-- -- foo:true
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" Lucene.'true'
-- @
true :: Lucene expr query => expr 'TBool
true = L.true

-- | A @false@ expression.
--
-- Example:
--
-- @
-- -- foo:false
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" Lucene.'false'
-- @
false :: Lucene expr query => expr 'TBool
false = L.false

-- | A single word. Must /not/ contain any spaces, wildcard characters
-- (@\'?\'@ and @\'*\'@), or tildes (@\'~\'@), though this is not enforced by
-- the type system.
--
-- Note that sometimes you may use the 'Data.String.IsString' instance for
-- 'Lucene.Query.LuceneExpr' 'TWord', but usually an explicit type signature
-- will be required (at the interpretation site or earlier).
--
-- Example:
--
-- @
-- -- foo:bar
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'word' "bar")
--
-- -- foo:bar
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" ("bar" :: Lucene.'Lucene.Query.LuceneExpr' 'TWord')
-- @
word :: Lucene expr query => Text -> expr 'TWord
word = L.word

-- | A single word that may contain wildcard characters (@\'?\'@ and @\'*\'@),
-- although the meaning of consecutive @\'*\'@s is probably ill-defined. Must
-- also /not/ contain any spaces or tildes (@\'~\'@), though this is not
-- enforced by the type system.
--
-- Example:
--
-- @
-- -- foo:b?r
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'wild' "b?r")
-- @
wild :: Lucene expr query => Text -> expr 'TWild
wild = L.wild

-- | A regular expression, whose syntax is described by
-- <http://lucene.apache.org/core/5_5_0/core/org/apache/lucene/util/automaton/RegExp.html?is-external=true>.
--
-- Note that the leading and trailing @\'/\'@ must be omitted. The regex
-- innards are not type checked in any way.
--
-- @
-- -- foo:\/[mb]oat\/
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'regex' "[mb]oat")
-- @
regex :: Lucene expr query => Text -> expr 'TRegex
regex = L.regex

-- | A phrase, composed of multiple (non-fuzzy) words, none of which may
-- contain wildcard characters. Both of these properties are enforced by the
-- type system, as long as the words themselves adhere to the 'word' contract.
-- The list should not be empty.
--
-- Note that sometimes you may use the 'GHC.Exts.IsList' instance for
-- 'Lucene.Query.LuceneExpr' 'TPhrase', but usually an explicit type signature
-- will be required (at the interpretation site or earlier).
--
-- Example:
--
-- @
-- -- foo:"bar baz"
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'phrase' ["bar", "baz"]) -- ok
--
-- -- foo:"bar b?z" (an invalid Lucene query)
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'phrase' ["bar", Lucene.'wild' "b?z"]) -- type error
--
-- -- foo:"bar b?z" (an invalid Lucene query)
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'phrase' ["bar", "b?z"]) -- breaks 'word' contract
-- @
--
-- Or, with @OverloadedLists@:
--
-- @
-- -- foo:"bar baz"
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (["bar", "baz"] :: Lucene.'Lucene.Query.LuceneExpr' 'TPhrase')
-- @
phrase :: Lucene expr query => [expr 'TWord] -> expr 'TPhrase
phrase = L.phrase

-- | The @\'~\'@ operator, which fuzzes its argument (either a word or phrase)
-- by a numeric amount.
--
-- This will have one of the following two types:
--
-- @
-- fuzz :: 'Lucene.Query.LuceneExpr' 'TWord'   -> Int -> 'Lucene.Query.LuceneExpr' 'TFuzzyWord'   -- Int must be 0, 1, or 2
-- fuzz :: 'Lucene.Query.LuceneExpr' 'TPhrase' -> Int -> 'Lucene.Query.LuceneExpr' 'TFuzzyPhrase' -- Int must be positive
-- @
--
-- Example:
--
-- @
-- -- foo:bar~1
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'fuzz' (Lucene.'word' "bar") 1)
--
-- -- foo:"bar baz qux"~10
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'fuzz' (Lucene.'phrase' ["bar", "baz", "qux"]) 10)
-- @
fuzz :: (Lucene expr query, FuzzableType a) => expr a -> Int -> expr (TFuzzed a)
fuzz = (L.~:)

-- | A range expression.
--
-- This will have one of the following two types:
--
-- @
-- to :: 'Boundary' ('Lucene.Query.LuceneExpr' 'TWord') -> 'Boundary' ('Lucene.Query.LuceneExpr' 'TWord') -> 'Lucene.Query.LuceneExpr' 'TRange'
-- to :: 'Boundary' ('Lucene.Query.LuceneExpr' 'TInt')  -> 'Boundary' ('Lucene.Query.LuceneExpr' 'TInt')  -> 'Lucene.Query.LuceneExpr' 'TRange'
-- @
--
-- Example:
--
-- @
-- -- foo:[5 TO 10}
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'incl' (Lucene.'int' 5) \`Lucene.to\` Lucene.'excl' (Lucene.'int' 10))
-- @
to :: (Lucene expr query, PrimType a) => Boundary (expr a) -> Boundary (expr a) -> expr 'TRange
to = L.to
infix 6 `to`

-- | The @\'^\'@ operator, which boosts its argument.
--
-- This will have one of the following two types:
--
-- @
-- boost :: 'Lucene.Query.LuceneExpr' 'TWord'   -> Float -> 'Lucene.Query.LuceneExpr' 'TBoostedWord'
-- boost :: 'Lucene.Query.LuceneExpr' 'TPhrase' -> Float -> 'Lucene.Query.LuceneExpr' 'TBoostedPhrase'
-- @
--
-- Example:
--
-- @
-- -- foo:bar^3.5
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'boost' (Lucene.'word' "bar") 3.5)
--
-- -- foo:"bar baz"^3.5
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'boost' (Lucene.'phrase' ["bar", "baz"]) 3.5)
-- @
boost :: (Lucene expr query, BoostableType a) => expr a -> Float -> expr (TBoosted a)
boost = (L.^:)

-- | A field query.
--
-- Example:
--
-- @
-- -- foo:bar
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'word' "bar")
-- @
field :: Lucene expr query => Text -> expr a -> query
field = (L.=:)

-- | An @AND@ query.
--
-- Example:
--
-- @
-- -- foo:bar AND baz:qux
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'word' "bar") \`Lucene.and\` Lucene.'field' "baz" (Lucene.'word' "qux")
-- @
and :: Lucene expr query => query -> query -> query
and = (L.&&:)
infixr 3 `and`

-- | An @OR@ query.
--
-- Example:
--
-- @
-- -- foo:bar OR baz:qux
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'word' "bar") \`Lucene.or\` Lucene.'field' "baz" (Lucene.'word' "qux")
-- @
or :: Lucene expr query => query -> query -> query
or = (L.||:)
infixr 2 `or`

-- | A @NOT@, @\'!\'@ or @\'-\'@ query.
--
-- Example:
--
-- @
-- -- foo:bar NOT baz:qux
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'word' "bar") \`Lucene.not\` Lucene.'field' "baz" (Lucene.'word' "qux")
-- @
not :: Lucene expr query => query -> query -> query
not = (L.-:)
infixr 1 `not`

-- | The @\'^=\'@ constant score operator.
--
-- This is given right-fixity to reject queries like @q ^= 1 ^= 2@, which may
-- very well be a valid Lucene query (I haven't tested), but are nonetheless
-- nonsense.
--
-- Example:
--
-- @
-- -- (foo:bar)^=3.5
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'score' (Lucene.'field' "foo" (Lucene.'word' "bar")) 3.5
-- @
score :: Lucene expr query => query -> Float -> query
score = (L.^=:)
infixr 4 `score`

-- | Short-hand for fuzzing a word by 2. This is the default behavior of a
-- Lucene @\'~\'@ operator without an integer added.
--
-- @
-- 'fuzzy' e = 'fuzz' e 2
-- @
--
-- Example:
--
-- @
-- -- foo:bar~
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'fuzzy' "bar")
-- @
fuzzy :: Lucene expr query => expr 'TWord -> expr 'TFuzzyWord
fuzzy = L.fuzzy

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
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'gt' (Lucene.'int' 5))
-- @
gt :: (Lucene expr query, PrimType a) => expr a -> expr 'TRange
gt = L.gt

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
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'gte' (Lucene.'int' 5))
-- @
gte :: (Lucene expr query, PrimType a) => expr a -> expr 'TRange
gte = L.gte

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
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'lt' (Lucene.'int' 5))
-- @
lt :: (Lucene expr query, PrimType a) => expr a -> expr 'TRange
lt = L.lt

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
-- query :: Lucene.'Lucene.Query.LuceneQuery'
-- query = Lucene.'field' "foo" (Lucene.'lte' (Lucene.'int' 5))
-- @
lte :: (Lucene expr query, PrimType a) => expr a -> expr 'TRange
lte = L.lte

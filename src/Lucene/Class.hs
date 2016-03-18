{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Lucene.Class
  (
    -- * Lucene language
    Lucene(..)
    -- * Derived combinators
  , (~:)
  , fuzzy
  , (^:)
  , gt
  , gte
  , lt
  , lte
    -- * Range expression helpers
  , Boundary(..)
  , incl
  , excl
  , star
  ) where

import Lucene.Type

import Data.Text (Text)

-- | The finally tagless Lucene class. This admits multiple interpreters, with
-- one (lazy 'Data.ByteString.Lazy.ByteString's) provided by this library, in
-- the "Lucene.Query" module.
--
-- For simplicity, the type signatures in the examples below monomorphise the
-- functions to use 'Lucene.Query.LuceneQuery' (and therefore
-- 'Lucene.Query.LuceneExpr', due to the functional dependency).
class Lucene expr query | query -> expr, expr -> query where
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
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'int' 5
  -- @
  int :: Int -> expr TInt

  -- | A @true@ expression.
  --
  -- Example:
  --
  -- @
  -- -- foo:true
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'true'
  -- @
  true :: expr TBool

  -- | A @false@ expression.
  --
  -- Example:
  --
  -- @
  -- -- foo:false
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'false'
  -- @
  false :: expr TBool

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
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'word' "bar"
  -- @
  --
  -- Or, with @OverloadedStrings@:
  --
  -- @
  -- -- foo:bar
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' ("bar" :: 'Lucene.Query.LuceneExpr' 'TWord')
  -- @
  word :: Text -> expr TWord

  -- | A single word that may contain wildcard characters (@\'?\'@ and @\'*\'@),
  -- although it must not begin with a @\'*\'@. Must also /not/ contain any
  -- spaces or tildes (@\'~\'@), though this is not enforced by the type system.
  --
  -- Example:
  --
  -- @
  -- -- foo:b?r
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'wild' "b?r"
  -- @
  wild :: Text -> expr TWild

  -- | A regular expression, whose syntax is described by
  -- <http://lucene.apache.org/core/5_5_0/core/org/apache/lucene/util/automaton/RegExp.html?is-external=true>.
  --
  -- Note that the leading and trailing @\'/\'@ must be omitted. The regex
  -- innards are not type checked in any way.
  --
  -- @
  -- -- foo:\/[mb]oat\/
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'regex' "[mb]oat"
  -- @
  regex :: Text -> expr TRegex

  -- | A phrase, composed of multiple (non-fuzzy) words, none of which may
  -- contain wildcard characters. Both of these properties are enforced by the
  -- type system, as long as the words themselves adhere to the 'word' contract.
  -- The list should not be empty.
  --
  -- Note that sometimes you may use the 'GHC.Exts.IsList' instance for
  -- 'Lucene.Query.LuceneExpr' 'TPhrase', but usually an explicit type signature
  -- will be required (at the interpretation site or earlier).
  --
  -- Example (with @OverloadedStrings@):
  --
  -- @
  -- -- foo:"bar baz"
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'phrase' ["bar", "baz"] -- ok
  --
  -- -- foo:"bar b?z" (an invalid Lucene query)
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'phrase' ["bar", 'wild' "b?z"] -- type error
  --
  -- -- foo:"bar b?z" (an invalid Lucene query)
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'phrase' ["bar", "b?z"] -- breaks 'word' contract
  -- @
  --
  -- Or, with @OverloadedLists@ and @OverloadedStrings@:
  --
  -- @
  -- -- foo:"bar baz"
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' (["bar", "baz"] :: 'Lucene.Query.LuceneExpr' 'TPhrase')
  -- @
  phrase :: [expr TWord] -> expr TPhrase

  -- | The @\'~\'@ operator, which fuzzes its argument (either a word or phrase)
  -- by a numeric amount.
  --
  -- This will have one of the following two types:
  --
  -- @
  -- fuzz :: 'Lucene.Query.LuceneExpr' 'TWord'   -> Int -> 'LuceneExpr' 'TFuzzyWord'   -- Int must be 0, 1, or 2
  -- fuzz :: 'Lucene.Query.LuceneExpr' 'TPhrase' -> Int -> 'LuceneExpr' 'TFuzzyPhrase' -- Int must be positive
  -- @
  --
  -- Example:
  --
  -- @
  -- -- foo:bar~1
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'fuzz' ('word' "bar") 1
  --
  -- -- foo:"bar baz qux"~10
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'fuzz' ('phrase' ["bar", "baz", "qux"]) 10
  -- @
  fuzz :: FuzzableType a => expr a -> Int -> expr (TFuzzed a)

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
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'to' ('incl' ('int' 5)) ('excl' ('int' 10))
  -- @
  to :: PrimType a => Boundary (expr a) -> Boundary (expr a) -> expr TRange
  infixr 5 `to`

  -- | A boost expression.
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
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" 'Lucene.Query.=:' 'boost' ('word' "bar") 3.5
  --
  -- -- foo:"bar baz"^3.5
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" 'Lucene.Query.=:' 'boost' ('phrase' ["bar", "baz"]) 3.5
  -- @
  boost :: BoostableType a => expr a -> Float -> expr (TBoosted a)

  -- | A field query.
  --
  -- Example:
  --
  -- @
  -- -- foo:bar
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'word' "bar"
  -- @
  (=:) :: Text -> expr a -> query
  infix 4 =:

  -- | An @AND@ query.
  --
  -- Example:
  --
  -- @
  -- -- foo:bar AND baz:qux
  -- query :: 'Lucene.Query.LuceneQuery'
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
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'word' "bar"
  --     '||:' "baz" '=:' 'word' "qux"
  -- @
  (||:) :: query -> query -> query
  infixr 2 ||:

  -- | A @NOT@ or @\'-\'@ query.
  --
  -- Example:
  --
  -- @
  -- -- foo:bar -baz:qux
  -- query :: 'Lucene.Query.LuceneQuery'
  -- query = "foo" '=:' 'word' "bar"
  --      '-:' "baz" '=:' 'word' "qux"
  -- @
  (-:) :: query -> query -> query
  infixr 1 -:

-- | An infix version of 'fuzz'.
(~:) :: (Lucene expr query, FuzzableType a) => expr a -> Int -> expr (TFuzzed a)
(~:) = fuzz
infixr 5 ~:

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
-- query :: 'Lucene.Query.LuceneQuery'
-- query = "foo" '=:' 'fuzzy' "bar"
-- @
fuzzy :: Lucene expr query => expr TWord -> expr TFuzzyWord
fuzzy e = e ~: 2

-- | An infix version of 'boost'.
(^:) :: (Lucene expr query, BoostableType a) => expr a -> Float -> expr (TBoosted a)
(^:) = boost
infixr 5 ^:

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
-- query :: 'Lucene.Query.LuceneQuery'
-- query = "foo" '=:' 'gt' ('int' 5)
-- @
gt :: (Lucene expr query, PrimType a) => expr a -> expr TRange
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
-- query :: 'Lucene.Query.LuceneQuery'
-- query = "foo" '=:' 'gt' ('int' 5)
-- @
gte :: (Lucene expr query, PrimType a) => expr a -> expr TRange
gte e = incl e `to` star

-- | Short-hand for a less-than range query.
--
-- @
-- 'lt' e = 'star' \`to\` 'excl' e
-- @
--
-- Example:
--
-- @
-- -- foo:<5
-- -- foo:[* TO 5}
-- query :: 'Lucene.Query.LuceneQuery'
-- query = "foo" '=:' 'lt' ('int' 5)
-- @
lt :: (Lucene expr query, PrimType a) => expr a -> expr TRange
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
-- query :: 'Lucene.Query.LuceneQuery'
-- query = "foo" '=:' 'lte' ('int' 5)
-- @
lte :: (Lucene expr query, PrimType a) => expr a -> expr TRange
lte e = star `to` incl e


-- | An inclusive or exclusive expression for use in a range query, built with
-- either 'incl', 'excl', or 'star'.
--
-- The constructors are exported for use in interpreters.
data Boundary a
  = Inclusive a
  | Exclusive a
  | Star

-- | Mark an expression as inclusive, for use in a range query.
incl :: Lucene expr query => expr a -> Boundary (expr a)
incl = Inclusive

-- | Mark an expression as exclusive, for use in a range query.
excl :: Lucene expr query => expr a -> Boundary (expr a)
excl = Exclusive

star :: Lucene expr query => Boundary (expr a)
star = Star

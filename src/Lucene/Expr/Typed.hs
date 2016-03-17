{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Lucene.Expr.Typed
  (
  -- * Expression type
    LuceneExpr
  -- * Expression construction
  , int
  , true
  , false
  , word
  , wild
  , regex
  , phrase
  , fuzz
  , (~:)
  , fuzzy
  , to
  , boost
  , (^:)
  , Boundary
  , incl
  , excl
  -- * Expression compilation
  , compileLuceneExpr
  ) where

import Lucene.Type

import Data.ByteString.Builder    (Builder)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Monoid
import Data.String                (IsString(..))
import Data.Text                  (Text)
import GHC.Exts                   (IsList(..))

import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString.Builder    as BS
import qualified Data.ByteString.Lazy.Char8 as BS


-- | A type-tagged Lucene expression.
data LuceneExpr :: LuceneType -> * where
  EInt    :: Int -> LuceneExpr TInt
  ETrue   :: LuceneExpr TBool
  EFalse  :: LuceneExpr TBool
  EWord   :: Text -> LuceneExpr TWord
  EWild   :: Text -> LuceneExpr TWild
  ERegex  :: Text -> LuceneExpr TRegex
  EPhrase :: [LuceneExpr TWord] -> LuceneExpr TPhrase
  EFuzz   :: FuzzableType a => LuceneExpr a -> Int -> LuceneExpr (TFuzzed a)
  ERange  :: PrimType a => Boundary (LuceneExpr a) -> Boundary (LuceneExpr a) -> LuceneExpr TRange
  EBoost  :: BoostableType a => LuceneExpr a -> Float -> LuceneExpr (TBoosted a)

instance Num (LuceneExpr TInt) where
  EInt x + EInt y = EInt (x + y)
  EInt x * EInt y = EInt (x * y)
  abs (EInt x) = EInt (abs x)
  signum (EInt x) = EInt (signum x)
  fromInteger i = EInt (fromIntegral i)
  negate (EInt x) = EInt (negate x)

instance IsString (LuceneExpr TWord) where
  fromString s = EWord (T.pack s)

instance IsList (LuceneExpr TPhrase) where
  type Item (LuceneExpr TPhrase) = LuceneExpr TWord

  fromList = EPhrase
  toList (EPhrase ws) = ws

-- * Expression construction

-- | An @int@ expression.
--
-- Note that sometimes you may use the 'Num' instance for 'LuceneExpr' 'TInt',
-- however, due to the interaction between type classes and GADTs, an explicit
-- type signature will usually be required.
--
-- Example:
--
-- @
-- -- foo:5
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'int' 5
-- @
--
-- Here's an example where only one integer has to be explicitly "wrapped" with
-- 'int', for better or for worse:
--
-- @
-- -- foo:[5 TO 6]
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'range' ('incl' ('int' 5)) ('incl' 6)
-- @
int :: Int -> LuceneExpr TInt
int = EInt

-- | A @true@ expression.
--
-- Example:
--
-- @
-- -- foo:true
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'true'
-- @
true :: LuceneExpr TBool
true = ETrue

-- | A @false@ expression.
--
-- Example:
--
-- @
-- -- foo:false
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'false'
-- @
false :: LuceneExpr TBool
false = EFalse

-- | A single word. Must /not/ contain any spaces, wildcard characters (@\'?\'@
-- and @\'*\'@), or tildes (@\'~\'@), though this is not enforced by
-- the type system.
--
-- Note that sometimes you may use the 'IsString' instance for 'LuceneExpr'
-- 'TWord', however, due to the interaction between type classes and GADTs, an
-- explicit type signature will usually be required.
--
-- Example:
--
-- @
-- -- foo:bar
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'word' "bar"
-- @
--
-- Or, with @OverloadedStrings@:
--
-- @
-- -- foo:bar
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' ("bar" :: 'LuceneExpr' 'TWord')
-- @
word :: Text -> LuceneExpr TWord
word = EWord

-- | A single word that may contain wildcard characters (@\'?\'@ and @\'*\'@),
-- although it must not begin with a @\'*\'@. Must also /not/ contain any spaces
-- or tildes (@\'~\'@), though this is not enforced by the type system.
--
-- Example:
--
-- @
-- -- foo:b?r
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'wild' "b?r"
-- @
wild :: Text -> LuceneExpr TWild
wild = EWild

-- | A regular expression, whose syntax is described by
-- <http://lucene.apache.org/core/5_5_0/core/org/apache/lucene/util/automaton/RegExp.html?is-external=true>.
--
-- Note that the leading and trailing @\'/\'@ must be omitted. The regex innards
-- are not type checked in any way.
--
-- @
-- -- foo:\/[mb]oat\/
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'regex' "[mb]oat"
-- @
regex :: Text -> LuceneExpr TRegex
regex = ERegex

-- | A phrase, composed of multiple (non-fuzzy) words, none of which may contain
-- wildcard characters. Both of these properties are enforced by the type
-- system, as long as the words themselves adhere to the 'word' contract. The
-- list should not be empty.
--
-- Note that sometimes you may use the 'IsList' instance for 'LuceneExpr'
-- 'TPhrase', however, due to the interaction between type classes and GADTs, an
-- explicit type signature will usually be required.
--
-- Example (with @OverloadedStrings@):
--
-- @
-- -- foo:"bar baz"
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'phrase' ["bar", "baz"] -- ok
--
-- -- foo:"bar b?z" (an invalid Lucene query)
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'phrase' ["bar", 'wild' "b?z"] -- type error
--
-- -- foo:"bar b?z" (an invalid Lucene query)
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'phrase' ["bar", "b?z"] -- breaks 'word' contract
-- @
--
-- Or, with @OverloadedLists@ and @OverloadedStrings@:
--
-- @
-- -- foo:"bar baz"
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' (["bar", "baz"] :: 'LuceneExpr' 'TPhrase')
-- @
phrase :: [LuceneExpr TWord] -> LuceneExpr TPhrase
phrase = EPhrase

-- | A @\'~\'@ operator, which fuzzes its argument (either a word or phrase) by a
-- numeric amount.
--
-- This will have one of the following two types:
--
-- @
-- fuzz :: 'LuceneExpr' 'TWord'   -> Int -> 'LuceneExpr' 'TFuzzyWord'   -- Int must be 0, 1, or 2
-- fuzz :: 'LuceneExpr' 'TPhrase' -> Int -> 'LuceneExpr' 'TFuzzyPhrase' -- Int must be positive
-- @
--
-- Example:
--
-- @
-- -- foo:bar~1
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'fuzz' ('word' "bar") 1
-- @
--
-- @
-- -- foo:"bar baz qux"~10
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'fuzz' ('phrase' ["bar", "baz", "qux"]) 10
-- @
fuzz :: FuzzableType a => LuceneExpr a -> Int -> LuceneExpr (TFuzzed a)
fuzz = EFuzz

-- | An infix version of 'fuzz'.
(~:) :: FuzzableType a => LuceneExpr a -> Int -> LuceneExpr (TFuzzed a)
(~:) = EFuzz
infixr 5 ~:

-- | Short-hand for fuzzing a word by 2. This is the default behavior of a
-- Lucene @\'~\'@ suffix without an integer added.
--
-- Example:
--
-- @
-- -- foo:bar~
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'fuzzy' "bar" -- equivalent to: 'word' "bar" '~:' 2
-- @
fuzzy :: LuceneExpr TWord -> LuceneExpr TFuzzyWord
fuzzy e = e ~: 2

-- | A range expression.
--
-- This will have one of the following two types:
--
-- @
-- to :: 'Boundary' ('LuceneExpr' 'TWord') -> 'Boundary' ('LuceneExpr' 'TWord') -> 'LuceneExpr' 'TRange'
-- to :: 'Boundary' ('LuceneExpr' 'TInt')  -> 'Boundary' ('LuceneExpr' 'TInt')  -> 'LuceneExpr' 'TRange'
-- @
--
-- Example:
--
-- @
-- -- foo:[5 TO 10}
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'to' ('incl' ('int' 5)) ('excl' ('int' 10))
-- @
to :: PrimType a => Boundary (LuceneExpr a) -> Boundary (LuceneExpr a) -> LuceneExpr TRange
to = ERange
infixr 5 `to`

-- | A boost expression.
--
-- This will have one of the following two types:
--
-- @
-- boost :: 'LuceneExpr' 'TWord'   -> Float -> 'LuceneExpr' 'TBoostedWord'
-- boost :: 'LuceneExpr' 'TPhrase' -> Float -> 'LuceneExpr' 'TBoostedPhrase'
-- @
--
-- Example:
--
-- @
-- -- foo:bar^3.5
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'boost' ('word' "bar") 3.5
-- @
--
-- @
-- -- foo:"bar baz"^3.5
-- query :: 'Lucene.Query.Typed.LuceneQuery'
-- query = "foo" 'Lucene.Query.Typed.=:' 'boost' ('phrase' ["bar", "baz"]) 3.5
-- @
boost :: BoostableType a => LuceneExpr a -> Float -> LuceneExpr (TBoosted a)
boost = EBoost

-- | An infix version of 'boost'.
(^:) :: BoostableType a => LuceneExpr a -> Float -> LuceneExpr (TBoosted a)
(^:) = EBoost
infixr 5 ^:


-- | An inclusive or exclusive expression for use in a range query, built with
-- either 'incl' or 'excl'.
data Boundary a
  = Inclusive a
  | Exclusive a

-- | Mark an expression as inclusive, for use in a range query.
incl :: LuceneExpr a -> Boundary (LuceneExpr a)
incl = Inclusive

-- | Mark an expression as exclusive, for use in a range query.
excl :: LuceneExpr a -> Boundary (LuceneExpr a)
excl = Exclusive

-- TODO: escape characters
--
-- | Compile a Lucene expression to a 'Builder'. Because a Lucene expression is
-- correct by construction, this function is total.
compileLuceneExpr :: LuceneExpr a -> Builder
compileLuceneExpr (EInt i) = BS.lazyByteString (BS.pack (show i))
compileLuceneExpr ETrue = "true"
compileLuceneExpr EFalse = "false"
compileLuceneExpr (EWord s) = T.encodeUtf8Builder s
compileLuceneExpr (EWild s) = T.encodeUtf8Builder s
compileLuceneExpr (ERegex s) = "/" <> T.encodeUtf8Builder s <> "/"
compileLuceneExpr (EPhrase ss) = "\"" <> foldMap compileLuceneExpr ss <> "\""
compileLuceneExpr (EFuzz e n) = compileLuceneExpr e <> "~" <> BS.lazyByteString (BS.pack (show n))
compileLuceneExpr (ERange r1 r2) =
  case (r1, r2) of
    (Inclusive e1, Inclusive e2) -> go '[' ']' e1 e2
    (Inclusive e1, Exclusive e2) -> go '[' '}' e1 e2
    (Exclusive e1, Inclusive e2) -> go '{' ']' e1 e2
    (Exclusive e1, Exclusive e2) -> go '{' '}' e1 e2
 where
  go :: Char -> Char -> LuceneExpr a -> LuceneExpr a -> Builder
  go c1 c2 e1 e2 =
    BS.char8 c1 <> compileLuceneExpr e1 <> " TO " <> compileLuceneExpr e2 <> BS.char8 c2
compileLuceneExpr (EBoost e n) = compileLuceneExpr e <> "^" <> BS.lazyByteString (BS.pack (show n))

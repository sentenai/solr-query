{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Lucene.Expr.Typed where

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


data LuceneExpr :: LuceneType -> * where
  EInt    :: Int -> LuceneExpr TInt
  ETrue   :: LuceneExpr TBool
  EFalse  :: LuceneExpr TBool
  EWord   :: Text -> LuceneExpr TWord
  EWild   :: Text -> LuceneExpr TWild
  ERegex  :: Text -> LuceneExpr TRegex
  EPhrase :: [LuceneExpr TWord] -> LuceneExpr TPhrase
  EFuzzy  :: FuzzableType a => LuceneExpr a -> Int -> LuceneExpr (TFuzzed a)
  ERange  :: PrimType a => Ranged (LuceneExpr a) -> Ranged (LuceneExpr a) -> LuceneExpr TRange
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


data Ranged a
  = Inclusive a
  | Exclusive a


-- | An @int@ expression.
--
-- Note that sometimes you may use the 'Num' instance for 'LuceneExpr' 'TInt',
-- however, due to the interaction between type classes and GADTs, an explicit
-- type signature will usually be required.
--
-- @
-- -- foo:5
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'int' 5
-- @
--
-- Here's an example where only one integer has to be explicitly "wrapped" with
-- 'int', for better or for worse:
--
-- @
-- -- foo:[5 TO 6]
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'incl' ('int' 5) `'to'` 'incl' 6
-- @
int :: Int -> LuceneExpr TInt
int = EInt

-- | A @true@ expression.
--
-- @
-- -- foo:true
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'true'
-- @
true :: LuceneExpr TBool
true = ETrue

-- | A @false@ expression.
--
-- @
-- -- foo:false
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'false'
-- @
false :: LuceneExpr TBool
false = EFalse

-- | A single word. Should *not* contain any spaces, wildcard characters (@?@
-- and @*@), or tildes (@~@), though this is not enforced by the type system.
--
-- Note that sometimes you may use the 'IsString' instance for 'LuceneExpr'
-- 'TWord', however, due to the interaction between type classes and GADTs, an
-- explicit type signature will usually be required.
--
-- @
-- -- foo:bar
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'word' "bar"
-- @
--
-- Or, with @OverloadedStrings@:
--
-- @
-- -- foo:bar
-- query :: 'LuceneQuery'
-- query = "foo" '=:' ("bar" :: 'LuceneExpr' 'TWord')
-- @
word :: Text -> LuceneExpr TWord
word = EWord

-- | A single word that may contain wildcard characters (@?@ and @*@), although
-- it should not begin with a @*@. Should also *not* contain any spaces or
-- tildes (@~@), though this is not enforced by the type system.
--
-- @
-- -- foo:b?r
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'wild' "b?r"
-- @
wild :: Text -> LuceneExpr TWild
wild = EWild

-- | A regular expression, whose syntax is described by
-- 'http://lucene.apache.org/core/5_5_0/core/org/apache/lucene/util/automaton/RegExp.html?is-external=true'.
--
-- Note that the leading and trailing @/@ must be omitted. The regex innards are
-- not type checked in any way.
--
-- @
-- -- foo:/[mb]oat/
-- query :: 'LuceneQuery'
-- query = 'regex' "[mb]oat"
-- @
regex :: Text -> LuceneExpr TRegex
regex = ERegex

-- | A phrase, composed of multiple (non-fuzzy) words, none of which may contain
-- wildcard characters. Both of these properties are enforced by the type
-- system, as long as the words themselves adhere to the 'word' contract.
--
-- Note that sometimes you may use the 'IsList instance for 'LuceneExpr'
-- 'TPhrase', however, due to the interaction between type classes and GADTs, an
-- explicit type signature will usually be required.
--
-- With @OverloadedStrings@:
--
-- @
-- -- foo:"bar baz"
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'phrase' ["bar", "baz"] -- ok
--
-- -- foo:"bar b?z" (an invalid Lucene query)
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'phrase' ["bar", 'wild' "b?z"] -- type error
--
-- -- foo:"bar b?z" (an invalid Lucene query)
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'phrase' ["bar", "b?z"] -- breaks 'word' contract
-- @
--
-- Or, with @OverloadedLists@ and @OverloadedStrings@:
--
-- @
-- -- foo:"bar baz"
-- query :: 'LuceneQuery'
-- query = "foo" '=:' (["bar", "baz"] :: 'LuceneExpr' 'TPhrase')
-- @
phrase :: [LuceneExpr TWord] -> LuceneExpr TPhrase
phrase = EPhrase


-- | A @~@ operator, which fuzzes its argument (either a word or phrase).
--
-- This will have one of the following two types:
--
-- @
-- (~:) :: 'LuceneExpr' 'TWord'   -> Int -> 'LuceneExpr' 'TFuzzyWord'
-- (~:) :: 'LuceneExpr' 'TPhrase' -> Int -> 'LuceneExpr' 'TFuzzyPhrase'
-- @
--
-- A word can only be fuzzed by @0@, @1@, or @2@. A phrase can be fuzzed by any
-- positive integer.
--
-- @
-- -- foo:bar~2
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'word' "bar" '~:' 2
-- @
--
-- @
-- -- foo:"bar baz qux"~10
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'phrase' ["bar", "baz", "qux"] '~:' 10
-- @
--
(~:) :: FuzzableType a => LuceneExpr a -> Int -> LuceneExpr (TFuzzed a)
(~:) = EFuzzy

-- | Short-hand for fuzzing a word by 2. This is the default behavior of a @~@
-- suffix without an integer added.
--
-- @
-- -- foo:bar~
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'fuzzy' "bar" -- equivalent to: 'word' "bar" '~:' 2
-- @
fuzzy :: LuceneExpr TWord -> LuceneExpr TFuzzyWord
fuzzy e = e ~: 2

-- | A range expression.
--
-- @
-- -- foo:[5 TO 10}
-- query :: 'LuceneQuery'
-- query = 'range' ('incl' ('int' 5)) ('excl' ('int' 10))
-- @
range :: PrimType a => Ranged (LuceneExpr a) -> Ranged (LuceneExpr a) -> LuceneExpr TRange
range = ERange

-- | An alias for 'range', to be used as an infix operator.
--
-- This will have one of the following two types:
--
-- @
-- range :: 'Ranged' ('LuceneExpr' 'TWord') -> 'Ranged' ('LuceneExpr' 'TWord') -> 'LuceneExpr' 'TRange'
-- range :: 'Ranged' ('LuceneExpr' 'TInt')  -> 'Ranged' ('LuceneExpr' 'TInt')  -> 'LuceneExpr' 'TRange'
-- @
--
-- @
-- -- foo:[5 TO 10}
-- query :: 'LuceneQuery'
-- query = 'range' ('incl' ('int' 5)) ('excl' ('int' 10))
-- @
to :: PrimType a => Ranged (LuceneExpr a) -> Ranged (LuceneExpr a) -> LuceneExpr TRange
to = ERange

-- | Mark an expression as inclusive, for use in a range query.
incl :: LuceneExpr a -> Ranged (LuceneExpr a)
incl = Inclusive

-- | Mark an expression as exclusive, for use in a range query.
excl :: LuceneExpr a -> Ranged (LuceneExpr a)
excl = Exclusive

boost :: BoostableType a => LuceneExpr a -> Float -> LuceneExpr (TBoosted a)
boost = EBoost

(^:) :: BoostableType a => LuceneExpr a -> Float -> LuceneExpr (TBoosted a)
(^:) = EBoost


-- TODO: escape characters
compileLuceneExpr :: LuceneExpr a -> Builder
compileLuceneExpr (EInt i) = BS.lazyByteString (BS.pack (show i))
compileLuceneExpr ETrue = "true"
compileLuceneExpr EFalse = "false"
compileLuceneExpr (EWord s) = T.encodeUtf8Builder s
compileLuceneExpr (EWild s) = T.encodeUtf8Builder s
compileLuceneExpr (ERegex s) = "/" <> T.encodeUtf8Builder s <> "/"
compileLuceneExpr (EPhrase ss) = foldMap compileLuceneExpr ss
compileLuceneExpr (EFuzzy e n) = compileLuceneExpr e <> "~" <> BS.lazyByteString (BS.pack (show n))
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

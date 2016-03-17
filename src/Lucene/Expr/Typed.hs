{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Lucene.Expr.Typed where

import Lucene.Type

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy.Char8    (ByteString)
import Data.Monoid
import Data.String             (IsString(..))
import Data.Text               (Text)
import GHC.Exts                (IsList(..))

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy.Char8    as BS


data Ranged a
  = Inclusive a
  | Exclusive a


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


int :: Int -> LuceneExpr TInt
int = EInt

true :: LuceneExpr TBool
true = ETrue

false :: LuceneExpr TBool
false = EFalse


-- | A single word. Should *not* contain any spaces, wildcard characters (@?@
-- and @*@), or tildes (@~@), though this is not enforced by the type system.
--
-- It may be more convenient to use the 'IsString' instance of 'LuceneExpr'
-- 'TWord', which requires the @OverloadedStrings@ language extension.
--
-- @
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'word' "bar"
-- @
--
-- Or, with @OverloadedStrings@:
--
-- @
-- query :: 'LuceneQuery'
-- query = "foo" '=:' "bar"
-- @
word :: Text -> LuceneExpr TWord
word = EWord

-- | A single word that may contain wildcard characters (@?@ and @*@), although
-- it should not begin with a @*@. Should also *not* contain any spaces or
-- tildes (@~@), though this is not enforced by the type system.
--
-- @
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'wild' "b?r"
-- @
wild :: Text -> LuceneExpr TWild
wild = EWild

regex :: Text -> LuceneExpr TRegex
regex = ERegex

-- | A phrase, composed of multiple (non-fuzzy) words, none of which may contain
-- wildcard characters. Both of these properties are enforced by the type
-- system, as long as the words themselves adhere to the 'word' contract.
--
-- It may be more convenient to use the 'IsList' instance of 'LuceneExpr'
-- 'TPhrase', which requires the @OverloadedLists@ language extension.
--
-- @
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'phrase' ['word' "bar", 'word' "baz"] -- ok
--
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'phrase' ['word' "bar", 'wild' "b?z"] -- type error
--
-- query :: 'LuceneQuery'
-- query = "foo" '=:' 'phrase' ['word' "bar", 'word' "b?z"] -- breaks 'word' contract
-- @
--
-- Or, with @OverloadedLists@ and @OverloadedStrings@:
--
-- @
-- query :: 'LuceneQuery'
-- query = "foo" '=:' ["bar", "baz"] -- ok
--
-- query :: 'LuceneQuery'
-- query = "foo" '=:' ["bar", "b?z"] -- breaks 'word' contract
-- @
phrase :: [LuceneExpr TWord] -> LuceneExpr TPhrase
phrase = EPhrase



(~:) :: FuzzableType a => LuceneExpr a -> Int -> LuceneExpr (TFuzzed a)
(~:) = EFuzzy

fuzzy :: LuceneExpr TWord -> LuceneExpr TFuzzyWord
fuzzy e = e ~: 2

range :: PrimType a => Ranged (LuceneExpr a) -> Ranged (LuceneExpr a) -> LuceneExpr TRange
range = ERange

to :: PrimType a => Ranged (LuceneExpr a) -> Ranged (LuceneExpr a) -> LuceneExpr TRange
to = ERange

incl :: LuceneExpr a -> Ranged (LuceneExpr a)
incl = Inclusive

excl :: LuceneExpr a -> Ranged (LuceneExpr a)
excl = Exclusive

boost :: BoostableType a => LuceneExpr a -> Float -> LuceneExpr (TBoosted a)
boost = EBoost

(^:) :: BoostableType a => LuceneExpr a -> Float -> LuceneExpr (TBoosted a)
(^:) = EBoost


compileLuceneQuery :: LuceneExpr a -> ByteString
compileLuceneQuery = BS.toLazyByteString . go
 where
  go :: LuceneExpr a -> Builder
  go (EInt i) = BS.lazyByteString (BS.pack (show i))
  go ETrue = "true"
  go EFalse = "false"
  go (EWord s) = T.encodeUtf8Builder s
  go (EWild s) = T.encodeUtf8Builder s
  go (ERegex s) = "/" <> T.encodeUtf8Builder s <> "/"
  go (EPhrase ss) = foldMap go ss
  go (EFuzzy e n) = go e <> "~" <> BS.lazyByteString (BS.pack (show n))
  go (ERange r1 r2) =
    case (r1, r2) of
      (Inclusive e1, Inclusive e2) -> go' '[' ']' e1 e2
      (Inclusive e1, Exclusive e2) -> go' '[' '}' e1 e2
      (Exclusive e1, Inclusive e2) -> go' '{' ']' e1 e2
      (Exclusive e1, Exclusive e2) -> go' '{' '}' e1 e2
   where
    go' :: Char -> Char -> LuceneExpr a -> LuceneExpr a -> Builder
    go' c1 c2 e1 e2 = BS.char8 c1 <> go e1 <> " TO " <> go e2 <> BS.char8 c2

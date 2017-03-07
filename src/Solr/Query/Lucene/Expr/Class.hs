{-# language CPP #-}

#if MIN_VERSION_base(4,9,0)
{-# options_ghc -fno-warn-redundant-constraints #-}
#endif

module Solr.Query.Lucene.Expr.Class where

import Solr.Query.Lucene.Expr.Type
import Solr.Prelude

import Data.String (IsString)

-- $setup
-- >>> import Data.Time (UTCTime(..), fromGregorian)
-- >>> import Solr.Query.Lucene

type LuceneExpr ty = forall expr. LuceneExprSYM expr => expr ty

-- | The @lucene@ expression language.
class IsString (expr 'TWord) => LuceneExprSYM expr where
  -- | An @int@ expression.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: int 5)
  -- "q={!lucene}foo:5"
  int :: Int64 -> expr 'TNum

  -- | A @float@ expression.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: float 5)
  -- "q={!lucene}foo:5.0"
  float :: Double -> expr 'TNum

  -- | A @true@ expression.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: true)
  -- "q={!lucene}foo:true"
  true :: expr 'TBool

  -- | A @false@ expression.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: false)
  -- "q={!lucene}foo:false"
  false :: expr 'TBool

  -- | A single word. Must /not/ contain any spaces, wildcard characters
  -- (@\'?\'@ and @\'*\'@), or tildes (@\'~\'@), though this is not enforced by
  -- the type system.
  --
  -- Note that sometimes you may use the 'Data.String.IsString' instance for
  -- 'Solr.Query.Expr' 'TWord', but usually an explicit type signature
  -- will be required (at the interpretation site or earlier).
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar")
  -- "q={!lucene}foo:bar"
  word :: Text -> expr 'TWord

  -- | A single word that may contain wildcard characters (@\'?\'@ and @\'*\'@),
  -- although the meaning of consecutive @\'*\'@s is probably ill-defined. Must
  -- also /not/ contain any spaces or tildes (@\'~\'@), though this is not
  -- enforced by the type system.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: wild "b?r")
  -- "q={!lucene}foo:b?r"
  wild :: Text -> expr 'TWild

  -- | A regular expression, whose syntax is described by
  -- <http://lucene.apache.org/core/5_5_0/core/org/apache/lucene/util/automaton/RegExp.html?is-external=true>.
  --
  -- Note that the leading and trailing @\'/\'@ must be omitted. The regex
  -- innards are not type checked in any way.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: regex "[mb]oat")
  -- "q={!lucene}foo:/[mb]oat/"
  regex :: Text -> expr 'TRegex

  -- | A phrase, composed of multiple (non-fuzzy) words, none of which may
  -- contain wildcard characters. Both of these properties are enforced by the
  -- type system, as long as the words themselves adhere to the 'word' contract.
  -- The list should not be empty.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: phrase ["bar", "baz"])
  -- "q={!lucene}foo:\"bar baz\""
  phrase :: [expr 'TWord] -> expr 'TPhrase

  -- | A 'DateTime' expression. This may either be a timestamp ('UTCTime'), or a
  -- "truncated" 'DateTime' such as @(2015, 5, 12)@.
  --
  -- ==== __Examples__
  --
  -- >>> let date = fromGregorian 2016 1 1
  -- >>> let time = fromIntegral 0
  -- >>> compile [] [] ("foo" =: datetime (UTCTime date time))
  -- "q={!lucene}foo:\"2016-01-01T00:00:00Z\""
  --
  -- >>> compile [] [] ("foo" =: datetime (2015 :: Year))
  -- "q={!lucene}foo:\"2015\""
  --
  -- >>> compile [] [] ("foo" =: datetime (2015, 1, 15, 11))
  -- "q={!lucene}foo:\"2015-01-15T11\""
  datetime :: IsDateTime a => a -> expr 'TDateTime

  -- | The @\'~\'@ operator, which fuzzes its argument (either a word or phrase)
  -- by a numeric amount.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar" ~: 1)
  -- "q={!lucene}foo:bar~1"
  --
  -- >>> compile [] [] ("foo" =: phrase ["bar", "baz", "qux"] ~: 10)
  -- "q={!lucene}foo:\"bar baz qux\"~10"
  (~:) :: Fuzzable a => expr a -> Int -> expr 'TFuzzy
  infix 6 ~:

  -- | A range expression.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: incl (int 5) `to` excl (int 10))
  -- "q={!lucene}foo:[5 TO 10}"
  --
  -- >>> compile [] [] ("foo" =: excl (word "bar") `to` star)
  -- "q={!lucene}foo:{bar TO *]"
  --
  -- >>> compile [] [] ("foo" =: star `to` star)
  -- "q={!lucene}foo:[* TO *]"
  to :: Rangeable a b => Boundary expr a -> Boundary expr b -> expr 'TRange
  infix 6 `to`

  -- | The @\'^\'@ operator, which boosts its argument.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar" ^: 3.5)
  -- "q={!lucene}foo:bar^3.5"
  --
  -- >>> compile [] [] ("foo" =: phrase ["bar", "baz"] ^: 3.5)
  -- "q={!lucene}foo:\"bar baz\"^3.5"
  (^:) :: Boostable a => expr a -> Float -> expr 'TBoosted
  infix 6 ^:

-- | Short-hand for fuzzing a word by 2. This is the default behavior of a
-- Solr @\'~\'@ operator without an integer added.
--
-- @
-- 'fuzzy' e = e '~:' 2
-- @
--
-- ==== __Examples__
--
-- >>> compile [] [] ("foo" =: fuzzy "bar")
-- "q={!lucene}foo:bar~2"
fuzzy :: LuceneExprSYM expr => expr 'TWord -> expr 'TFuzzy
fuzzy e = e ~: 2

-- | Short-hand for a greater-than range query.
--
-- @
-- 'gt' e = 'excl' e \`to\` 'star'
-- @
--
-- ==== __Examples__
--
-- >>> compile [] [] ("foo" =: gt (int 5))
-- "q={!lucene}foo:{5 TO *]"
gt :: (LuceneExprSYM expr, Rangeable a 'TAny) => expr a -> expr 'TRange
gt e = excl e `to` star

-- | Short-hand for a greater-than-or-equal-to range query.
--
-- @
-- 'gte' e = 'incl' e \`to\` 'star'
-- @
--
-- ==== __Examples__
--
-- >>> compile [] [] ("foo" =: gte (int 5))
-- "q={!lucene}foo:[5 TO *]"
gte :: (LuceneExprSYM expr, Rangeable a 'TAny) => expr a -> expr 'TRange
gte e = incl e `to` star

-- | Short-hand for a less-than range query.
--
-- @
--  'lt' e = 'star' \`to\` 'excl' e
-- @
--
-- ==== __Examples__
--
-- >>> compile [] [] ("foo" =: lt (int 5))
-- "q={!lucene}foo:[* TO 5}"
lt :: (LuceneExprSYM expr, Rangeable 'TAny a) => expr a -> expr 'TRange
lt e = star `to` excl e

-- | Short-hand for a less-than-or-equal-to range query.
--
-- @
-- 'lte' e = 'star' \`to\` 'incl' e
-- @
--
-- ==== __Examples__
--
-- >>> compile [] [] ("foo" =: lte (int 5))
-- "q={!lucene}foo:[* TO 5]"
lte :: (LuceneExprSYM expr, Rangeable 'TAny a) => expr a -> expr 'TRange
lte e = star `to` incl e

-- | An inclusive or exclusive expression for use in a range query, built with
-- either 'incl', 'excl', or 'star'.
--
-- The constructors are exported for use in interpreters.
data Boundary expr ty where
  Inclusive :: expr ty -> Boundary expr ty
  Exclusive :: expr ty -> Boundary expr ty
  Star :: Boundary expr 'TAny

deriving instance Eq   (expr ty) => Eq   (Boundary expr ty)
deriving instance Show (expr ty) => Show (Boundary expr ty)

-- | Mark an expression as inclusive, for use in a range query.
incl :: LuceneExprSYM expr => expr a -> Boundary expr a
incl = Inclusive

-- | Mark an expression as exclusive, for use in a range query.
excl :: LuceneExprSYM expr => expr a -> Boundary expr a
excl = Exclusive

-- | @\'*\'@ operator, signifying the minimum or maximun bound of a range.
star :: LuceneExprSYM expr => Boundary expr 'TAny
star = Star

-- | Named version of ('~:').
fuzz :: (LuceneExprSYM expr, Fuzzable a) => expr a -> Int -> expr 'TFuzzy
fuzz = (~:)

-- | Named version of ('^:').
boost :: (LuceneExprSYM expr, Boostable a) => expr a -> Float -> expr 'TBoosted
boost = (^:)


-- | 'DateTime' literals. 'DateTime' expressions are constructed using the
-- internal 'IsDateTime' typeclass, for which there exist the following
-- instances:
--
-- @
-- instance 'IsDateTime' 'UTCTime'
-- instance 'IsDateTime' 'Year'
-- instance 'IsDateTime' ('Year', 'Month')
-- instance 'IsDateTime' ('Year', 'Month', 'Day')
-- instance 'IsDateTime' ('Year', 'Month', 'Day', 'Hour')
-- instance 'IsDateTime' ('Year', 'Month', 'Day', 'Hour', 'Minute')
-- instance 'IsDateTime' ('Year', 'Month', 'Day', 'Hour', 'Minute', 'Second')
-- instance 'IsDateTime' ('Year', 'Month', 'Day', 'Hour', 'Minute', 'Second', 'Millisecond')
-- @
data DateTime
  = UTC UTCTime
  | Truncated TruncatedDateTime

type TruncatedDateTime
  = (Year, Maybe (Month, Maybe (Day, Maybe (Hour, Maybe (Minute, Maybe (Second, Maybe Millisecond))))))

type Leg a b = (a, Maybe b)

type Y   = Leg Year   M
type M   = Leg Month  D
type D   = Leg Day    H
type H   = Leg Hour   Min
type Min = Leg Minute S
type S   = Leg Second Millisecond

-- | Year.
type Year = Int

-- | @1@-indexed month. Clamped to the range @1-12@.
type Month = Int

-- | @1@-indexed day. Clamped to the range @1-31@.
type Day = Int

-- | Hour. Clamped to the range @0-23@.
type Hour = Int

-- | Minute. Clamped to the range @0-59@.
type Minute = Int

-- | Second. Clamped to the range @0-60@.
type Second = Int

-- | Millisecond. Clamped to the range @0-99.999@.
type Millisecond = Double

class IsDateTime a where
  toDateTime :: a -> DateTime

instance IsDateTime UTCTime where
  toDateTime = UTC

instance IsDateTime Year where
  toDateTime a = mkY a Nothing

instance (a ~ Year, b ~ Month) => IsDateTime (a, b) where
  toDateTime (a, b) = mkY a (mkM b Nothing)

instance (a ~ Year, b ~ Month, c ~ Day) => IsDateTime (a, b, c) where
  toDateTime (a, b, c) = mkY a (mkM b (mkD c Nothing))

instance (a ~ Year, b ~ Month, c ~ Day, d ~ Hour) => IsDateTime (a, b, c, d) where
  toDateTime (a, b, c, d) = mkY a (mkM b (mkD c (mkH d Nothing)))

instance (a ~ Year, b ~ Month, c ~ Day, d ~ Hour, e ~ Minute) => IsDateTime (a, b, c, d, e) where
  toDateTime (a, b, c, d, e) = mkY a (mkM b (mkD c (mkH d (mkMin e Nothing))))

instance (a ~ Year, b ~ Month, c ~ Day, d ~ Hour, e ~ Minute, f ~ Second) => IsDateTime (a, b, c, d, e, f) where
  toDateTime (a, b, c, d, e, f) = mkY a (mkM b (mkD c (mkH d (mkMin e (mkS f Nothing)))))

instance (a ~ Year, b ~ Month, c ~ Day, d ~ Hour, e ~ Minute, f ~ Second, g ~ Millisecond) => IsDateTime (a, b, c, d, e, f, g) where
  toDateTime (a, b, c, d, e, f, g) = mkY a (mkM b (mkD c (mkH d (mkMin e (mkS f (mkMilli g))))))

mkY :: Year -> Maybe M -> DateTime
mkY a b = Truncated (a, b)

mkM :: Month -> Maybe D -> Maybe M
mkM a b = Just (clamp 1 12 a, b)

mkD :: Day -> Maybe H -> Maybe D
mkD a b = Just (clamp 1 31 a, b)

mkH :: Hour -> Maybe Min -> Maybe H
mkH a b = Just (clamp 0 23 a, b)

mkMin :: Minute -> Maybe S -> Maybe Min
mkMin a b = Just (clamp 0 59 a, b)

mkS :: Second -> Maybe Millisecond -> Maybe S
mkS a b = Just (clamp 0 60 a, b)

mkMilli :: Millisecond -> Maybe Millisecond
mkMilli a = Just (clamp 0 99.999 a)

clamp :: Ord a => a -> a -> a -> a
clamp a z = min z . max a

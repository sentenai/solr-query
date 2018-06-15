{-# language CPP #-}

#if MIN_VERSION_base(4,9,0)
{-# options_ghc -fno-warn-redundant-constraints #-}
#endif

module Solr.Query.Lucene.Expr where

import Builder
import Solr.Prelude
import Solr.Query.Lucene.Expr.Type

import Data.String (IsString(..))

import qualified Data.Text as Text

-- | A @lucene@ expression.
newtype LuceneExpr (t :: LuceneExprTy) = E { unE :: Builder }
  deriving (Eq, Show)

instance IsString (LuceneExpr 'TWord) where
  fromString s = word (pack s)

-- | An @int@ expression.
int :: Int64 -> LuceneExpr 'TNum
int n = E (bshow n)

-- | A @float@ expression.
float :: Double -> LuceneExpr 'TNum
float n = E (bshow n)

-- | A @true@ expression.
true :: LuceneExpr 'TBool
true = E "true"

-- | A @false@ expression.
false :: LuceneExpr 'TBool
false = E "false"

-- | A single word. Must /not/ contain any spaces, wildcard characters
-- (@\'?\'@ and @\'*\'@), or tildes (@\'~\'@), though this is not enforced by
-- the type system.
--
-- Note that sometimes you may use the 'Data.String.IsString' instance for
-- 'LuceneExpr' 'TWord', but usually an explicit type signature
-- will be required (at the interpretation site or earlier).
word :: Text -> LuceneExpr 'TWord
word s =
  if Text.null s
    then E "\"\""
    else E (thaw' s)

-- | A fuzzy word.
fuzzy :: Text -> Int -> LuceneExpr 'TWord
fuzzy s n = E (mconcat [thaw' s, char '~', bshow (max 0 (min 2 n))])

-- | A single word that may contain wildcard characters (@\'?\'@ and @\'*\'@),
-- although the meaning of consecutive @\'*\'@s is probably ill-defined. Must
-- also /not/ contain any spaces or tildes (@\'~\'@), though this is not
-- enforced by the type system.
wild :: Text -> LuceneExpr 'TWild
wild s = E (thaw' s)

-- | A <https://lucene.apache.org/core/6_4_2/core/org/apache/lucene/util/automaton/RegExp.html regular expression>.
--
-- Note that the leading and trailing @\'/\'@ must be omitted. The regex
-- innards are not type checked in any way.
regex :: Text -> LuceneExpr 'TRegex
regex s = E (mconcat [char '/', thaw' s, char '/'])

-- | A phrase, composed of multiple (non-fuzzy) words, none of which may
-- contain wildcard characters. Both of these properties are enforced by the
-- type system, as long as the words themselves adhere to the 'word' contract.
-- The list should not be empty.
phrase :: [LuceneExpr 'TWord] -> LuceneExpr 'TPhrase
phrase ss = E (dquotes (intersperse ' ' (map unE ss)))

-- | A proximity phrase.
proximity :: Int -> [LuceneExpr 'TWord] -> LuceneExpr 'TPhrase
proximity n ss = E (mconcat
  [ dquotes (intersperse ' ' (map unE ss))
  , char '~'
  , bshow (max 0 n)
  ])

-- | A 'DateTime' expression. This may either be a timestamp ('UTCTime'), or a
-- "truncated" 'DateTime' such as @(2015, 5, 12)@.
datetime :: IsDateTime a => a -> LuceneExpr 'TDateTime
datetime t =
  case toDateTime t of
    UTC t' ->
      E (thawStr (formatTime defaultTimeLocale "\"%Y-%m-%dT%H:%M:%S%QZ\"" t'))
    Truncated t' -> E (formatTruncated t')
 where
  formatTruncated :: TruncatedDateTime -> Builder
  formatTruncated =
    go '"' show
      (go '-' fmt
        (go '-' fmt
          (go 'T' fmt
            (go ':' fmt
              (go ':' fmt formatMilli)))))
   where
    go :: Char -> (a -> String) -> (b -> Builder) -> (a, Maybe b) -> Builder
    go c f g (a, b) = mconcat [char c, thawStr (f a), maybe (char '"') g b]

    fmt :: Int -> String
    fmt = printf "%02d"

  -- Format to 5 decimal places
  formatMilli :: Millisecond -> Builder
  formatMilli ml = thawStr (tail (printf "%.5f" (ml / 100))) <> "Z\""

-- | A range expression.
to :: Rangeable a b => Boundary a -> Boundary b -> LuceneExpr 'TRange
to b1 b2 = E (mconcat [lhs b1, " TO ", rhs b2])
 where
  lhs :: Boundary a -> Builder
  lhs (Inclusive e) = char '[' <> unE e
  lhs (Exclusive e) = char '{' <> unE e
  lhs Star          = "[*"

  rhs :: Boundary a -> Builder
  rhs (Inclusive e) = unE e <> char ']'
  rhs (Exclusive e) = unE e <> char '}'
  rhs Star          = "*]"
infix 9 `to`

-- | Short-hand for a greater-than range query.
--
-- @
-- 'gt' e = 'excl' e \`to\` 'star'
-- @
gt :: Rangeable a 'TAny => LuceneExpr a -> LuceneExpr 'TRange
gt e = excl e `to` star

-- | Short-hand for a greater-than-or-equal-to range query.
--
-- @
-- 'gte' e = 'incl' e \`to\` 'star'
-- @
gte :: Rangeable a 'TAny => LuceneExpr a -> LuceneExpr 'TRange
gte e = incl e `to` star

-- | Short-hand for a less-than range query.
--
-- @
--  'lt' e = 'star' \`to\` 'excl' e
-- @
lt :: Rangeable 'TAny a => LuceneExpr a -> LuceneExpr 'TRange
lt e = star `to` excl e

-- | Short-hand for a less-than-or-equal-to range query.
--
-- @
-- 'lte' e = 'star' \`to\` 'incl' e
-- @
lte :: Rangeable 'TAny a => LuceneExpr a -> LuceneExpr 'TRange
lte e = star `to` incl e

-- | An inclusive or exclusive expression for use in a range query, built with
-- either 'incl', 'excl', or 'star'.
--
-- The constructors are exported for use in interpreters.
data Boundary ty where
  Inclusive :: LuceneExpr ty -> Boundary ty
  Exclusive :: LuceneExpr ty -> Boundary ty
  Star :: Boundary ty

deriving instance Eq (Boundary ty)
deriving instance Show (Boundary ty)

-- | Mark an expression as inclusive, for use in a range query.
incl :: LuceneExpr a -> Boundary a
incl = Inclusive

-- | Mark an expression as exclusive, for use in a range query.
excl :: LuceneExpr a -> Boundary a
excl = Exclusive

-- | @\'*\'@ operator, signifying the minimum or maximum bound of a range.
star :: Boundary 'TAny
star = Star

-- | Returns 'Star' if Nothing
fromMaybeStar :: Maybe (Boundary a) -> Boundary a
fromMaybeStar = fromMaybe Star

-- | @\'Intersects\'@ spatial predicate.
intersects :: Shape -> LuceneExpr 'TSpatialPredicate
intersects (S s) = E (dquotes ("Intersects" <> parens s))

-- | @\'IsWithin\'@ spatial predicate.
isWithin :: Shape -> LuceneExpr 'TSpatialPredicate
isWithin (S s) = E (mconcat ["IsWithin(", s, char ')'])

-- | A shape.
newtype Shape
  = S Builder

-- | A @POLYGON@ shape.
polygon :: [(Double, Double)] -> Shape
polygon =
  S . ("POLYGON" <>) . parens . intersperse ',' .
    map (\(x, y) -> bshow x <> char ' ' <> bshow y)

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

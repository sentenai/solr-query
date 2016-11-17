module Solr.DateTime.ReallyInternal where

import Data.Time.Clock (UTCTime)

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

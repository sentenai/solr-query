module Solr.Query.Geofilt
  ( -- * Geofilt query
    GeofiltQuery
    -- ** Local params
  , Latitude
  , Longitude
  , d
  , pt
  , sfield
  ) where

import Solr.Prelude

import Builder
import Solr.Query.Internal.Internal

type Latitude = Double
type Longitude = Double

-- | A 'GeofiltQuery' exists only through its 'LocalParams'. Use 'def' to
-- construct a 'GeofiltQuery' to 'Solr.Query.compile'.
newtype GeofiltQuery
  = Q { unQ :: Builder }

instance Default GeofiltQuery where
  def :: GeofiltQuery
  def = Q mempty

instance Query GeofiltQuery where
  data LocalParams GeofiltQuery = GeofiltParams
    { _d :: Maybe Double
    , _pt :: Maybe (Latitude, Longitude)
    , _sfield :: Maybe Text
    }

  compileLocalParams :: LocalParams GeofiltQuery -> [(Builder, Builder)]
  compileLocalParams GeofiltParams{_d, _pt, _sfield} =
    ("type", "geofilt") : catMaybes
      [ compileD <$> _d
      , compilePt <$> _pt
      , compileSfield <$> _sfield
      ]
   where
    compileD :: Double -> (Builder, Builder)
    compileD n = ("d", bshow n)

    compilePt :: (Latitude, Longitude) -> (Builder, Builder)
    compilePt (n, m) = ("pt", bshow n <> char ',' <> bshow m)

    compileSfield :: Text -> (Builder, Builder)
    compileSfield s = ("sfield", thaw' s)

instance Default (LocalParams GeofiltQuery) where
  def :: LocalParams GeofiltQuery
  def = GeofiltParams Nothing Nothing Nothing

-- | The @\'d\'@ local parameter.
d :: Double -> LocalParams GeofiltQuery -> LocalParams GeofiltQuery
d x s = s { _d = Just x }

-- | The @\'pt\'@ local parameter.
pt
  :: Latitude -> Longitude -> LocalParams GeofiltQuery
  -> LocalParams GeofiltQuery
pt x y s = s { _pt = Just (x, y) }

-- | The @\'sfield\'@ local parameter.
sfield :: Text -> LocalParams GeofiltQuery -> LocalParams GeofiltQuery
sfield x s = s { _sfield = Just x }

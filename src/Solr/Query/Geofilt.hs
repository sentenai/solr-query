{-# LANGUAGE TupleSections #-}
module Solr.Query.Geofilt
  ( -- * Geofilt query
    GeofiltQuery
    -- ** Local params
  , d
  , pt
  , sfield
  ) where

import Solr.Prelude

import Builder
import Solr.Query.Internal.Internal

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
    , _pt :: Maybe [Double]
    , _sfield :: Maybe Text
    }

  compileLocalParams :: LocalParams GeofiltQuery -> [(Builder, Builder)]
  compileLocalParams GeofiltParams{_d, _pt, _sfield} =
    ("type", "geofilt") : catMaybes
      [ compileD <$> _d
      , ("pt",) . compilePt <$> _pt
      , compileSfield <$> _sfield
      ]
   where
    compileD :: Double -> (Builder, Builder)
    compileD n = ("d", bshow n)

    compilePt :: [Double] -> Builder
    compilePt [] = ""
    compilePt [a] = bshow a
    compilePt (a:as) = bshow a <> char ',' <> compilePt as

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
  :: [Double] -> LocalParams GeofiltQuery
  -> LocalParams GeofiltQuery
pt p s = s { _pt = Just p }

-- | The @\'sfield\'@ local parameter.
sfield :: Text -> LocalParams GeofiltQuery -> LocalParams GeofiltQuery
sfield x s = s { _sfield = Just x }

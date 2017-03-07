module Solr.Query.Geofilt.Class where

import Solr.Prelude
import Solr.Query (Query)
import Solr.Query.Param.Local (LocalParam)

-- $setup
-- >>> import Solr.Query.Geodist

type Latitude = Double
type Longitude = Double

-- | A 'GeofiltQuery' is a 'Query' built from the 'GeofiltQuerySYM' language.
type GeofiltQuery = Query GeofiltQuerySYM

data instance LocalParam GeofiltQuerySYM
  = D Double
  | Pt Latitude Longitude
  | Sfield Text
  deriving Show

type GeofiltQueryParam = LocalParam GeofiltQuerySYM

-- | The @geofilt@ query language.
class GeofiltQuerySYM (query :: *) where
  geofilt :: query

-- | The @\'d\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [d 1.5]
-- "q={!geofilt d=1.5}"
d :: Double -> GeofiltQueryParam
d = D

-- | The @\'pt\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [pt 2.5 3.5] mempty
-- "q={!geofilt pt=2.5,3.5}"
pt :: Latitude -> Longitude -> GeofiltQueryParam
pt = Pt

-- | The @\'sfield\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [sfield "foo"] mempty
-- "q={!geofilt sfield=foo}"
sfield :: Text -> GeofiltQueryParam
sfield = Sfield

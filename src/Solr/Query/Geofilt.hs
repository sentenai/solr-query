module Solr.Query.Geofilt
  ( -- * Interpreting a query
    compile
    -- * Geofilt query language
  , GeofiltQuery
  , GeofiltQuerySYM(..)
  , Latitude
  , Longitude
    -- * Local query parameters
  , GeofiltQueryParam
  , d
  , pt
  , sfield
  ) where

import Solr.Query.Geofilt.Class
import Solr.Query.Geofilt.Impl.Final (compile)

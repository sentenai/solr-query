{-# options_ghc -fno-warn-orphans #-}

module Solr.Query.Geofilt.Impl.Final where

import Builder
import Solr.Prelude
import Solr.Query (InterpretQuery(..))
import Solr.Query.Geofilt.Class
import Solr.Query.Param (Param)
import Solr.Query.Utils (compileParams)

import qualified Data.Text.Lazy

data Q = Q

instance GeofiltQuerySYM Q where
  geofilt :: Q
  geofilt = Q

instance InterpretQuery GeofiltQuerySYM Builder where
  interpretParams :: [GeofiltQueryParam] -> Builder
  interpretParams [] = "geofilt"
  interpretParams ps =
    "geofilt " <> intersperse ' ' (map compileLocalParam ps)

  interpretQuery :: Proxy GeofiltQuerySYM -> GeofiltQuery -> Builder
  interpretQuery _ _ = mempty

-- | Compile a 'GeofiltQuery' (which only consists of 'Param's and
-- 'GeofiltQueryQueryParam's) to a lazy 'Data.Text.Lazy.Text'.
--
-- Normally, you would 'Solr.Query.Lucene.compile' a @lucene@ query with a
-- @geofilt@ 'fq' parameter, rather than compile a @geofilt@ query directly.
compile :: [Param Builder] -> [GeofiltQueryParam] -> Data.Text.Lazy.Text
compile params locals =
  freeze
    (compileParams params <> "q={!" <> interpretParams locals
      <> char '}')

compileLocalParam :: GeofiltQueryParam -> Builder
compileLocalParam = \case
  D n -> "d=" <> bshow n
  Pt lat lon -> "pt=" <> bshow lat <> char ',' <> bshow lon
  Sfield s -> "sfield=" <> thaw' s

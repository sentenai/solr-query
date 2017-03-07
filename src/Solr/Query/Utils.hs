module Solr.Query.Utils where

import Builder
import Solr.Prelude
import Solr.Query
import Solr.Query.Param
import Solr.Query.Param.Local

-- | Compile a 'Param' to a 'Builder'. Usually 'compile' is more convenient.
--
-- ==== __Examples__
--
-- >>> compileParam (rows 5)
-- "rows=5"
compileParam :: Param Builder -> Builder
compileParam = \case
  ParamFl s -> "fl=" <> thaw' s
  ParamFq cache cost (locals :: [LocalParam sym]) (query :: Query sym) ->
    "fq={!" <> interpretParams locals <> " cache=" <> compileCache cache <>
      compileCost cost <> char '}' <> interpretQuery (Proxy :: Proxy sym) query
  ParamRows n -> "rows=" <> bshow n
  ParamSortAsc s -> "sort=" <> thaw' s <> " asc"
  ParamSortDesc s -> "sort=" <> thaw' s <> " desc"
  ParamStart n -> "start=" <> bshow n
 where
  compileCache :: Cache -> Builder
  compileCache Cache = "true"
  compileCache DontCache = "false"

  compileCost :: Maybe Cost -> Builder
  compileCost = maybe mempty (\cost -> " cost=" <> bshow cost)

compileParams :: [Param Builder] -> Builder
compileParams = foldr (\p b -> compileParam p <> char '&' <> b) mempty

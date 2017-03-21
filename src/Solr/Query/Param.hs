module Solr.Query.Param where

import Solr.Prelude

import Builder
import Solr.Query.Filter.Internal
import Solr.Query.Internal.Internal

-- | A query parameter.
data Param where
  ParamFl :: Text -> Param
  ParamFq :: Query query => FilterParams query -> query -> Param
  ParamRows :: Int -> Param
  ParamSortAsc :: Text -> Param
  ParamSortDesc :: Text -> Param
  ParamStart :: Int -> Param

-- | The @\'fl\'@ query parameter.
fl :: Text -> Param
fl = ParamFl

-- | The @\'fq\'@ query parameter.
fq :: Query query => FilterParams query -> query -> Param
fq = ParamFq

-- | The @\'rows\'@ query parameter.
rows :: Int -> Param
rows = ParamRows

-- | The @\'sort\'@ query parameter (ascending).
sortAsc :: Text -> Param
sortAsc = ParamSortAsc

-- | The @\'sort\'@ query parameter (descending).
sortDesc :: Text -> Param
sortDesc = ParamSortDesc

-- | The @\'start\'@ query parameter.
start :: Int -> Param
start = ParamStart

-- | Compile a 'Param' to a its ('Builder', 'Builder') equivalent.
compileParam :: Param -> (Builder, Builder)
compileParam = \case
  ParamFl s            -> ("fl",    thaw' s)
  ParamFq locals query -> ("fq",    compileFilterQuery locals query)
  ParamRows n          -> ("rows",  bshow n)
  ParamSortAsc s       -> ("sort",  thaw' s <> " asc")
  ParamSortDesc s      -> ("sort",  thaw' s <> " desc")
  ParamStart n         -> ("start", bshow n)

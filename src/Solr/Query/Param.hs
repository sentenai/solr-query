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
  ParamSort :: [(Text, SortWay)] -> Param
  ParamStart :: Int -> Param

data SortWay
  = Asc
  | Desc

-- | The @\'fl\'@ query parameter.
fl :: Text -> Param
fl = ParamFl

-- | The @\'fq\'@ query parameter.
fq :: Query query => FilterParams query -> query -> Param
fq = ParamFq

-- | The @\'rows\'@ query parameter.
rows :: Int -> Param
rows = ParamRows

-- | The @\'sort\'@ query parameter.
sort :: [(Text, SortWay)] -> Param
sort = ParamSort

-- | The @\'start\'@ query parameter.
start :: Int -> Param
start = ParamStart

-- | Compile a 'Param' to a its ('Builder', 'Builder') equivalent.
compileParam :: Param -> (Builder, Builder)
compileParam = \case
  ParamFl s            -> ("fl",    thaw' s)
  ParamFq locals query -> ("fq",    compileFilterQuery locals query)
  ParamRows n          -> ("rows",  bshow n)
  ParamSort ss         -> ("sort",  intersperse ',' (map compileSortParam ss))
  ParamStart n         -> ("start", bshow n)

compileSortParam :: (Text, SortWay) -> Builder
compileSortParam = \case
  (s, Asc)  -> thaw' s <> " asc"
  (s, Desc) -> thaw' s <> " desc"

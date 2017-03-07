module Solr.Query.Param where

import Solr.Prelude
import Solr.Query (InterpretQuery, Query)
import Solr.Query.Param.Local (LocalParam)

-- $setup
-- >>> import Solr.Query.Lucene

data Cache
  = Cache
  | DontCache
  deriving (Eq, Show)

type Cost = Int

-- | A common query parameter.
data Param a where
  ParamFl :: Text -> Param a
  ParamFq :: InterpretQuery sym a
          => Cache -> Maybe Cost -> [LocalParam sym] -> Query sym -> Param a
  ParamRows :: Int -> Param a
  ParamSortAsc :: Text -> Param a
  ParamSortDesc :: Text -> Param a
  ParamStart :: Int -> Param a

-- | The @\'fl\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> compile [fl "baz", fl "qux"] [] ("foo" =: word "bar")
-- "fl=baz&fl=qux&q={!lucene}foo:bar"
fl :: Text -> Param a
fl = ParamFl

-- | The @\'fq\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> let filterQuery = "baz" =: gte (int 10)
-- >>> let params = [] :: [LuceneQueryParam]
-- >>> compile [fq DontCache Nothing params filterQuery] [] ("foo" =: word "bar")
-- "fq={!lucene cache=false}baz:[10 TO *]&q={!lucene}foo:bar"
fq :: InterpretQuery sym a
   => Cache -> Maybe Cost -> [LocalParam sym] -> Query sym -> Param a
fq = ParamFq

-- | The @\'rows\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> compile [rows 5] [] ("foo" =: word "bar")
-- "rows=5&q={!lucene}foo:bar"
rows :: Int -> Param a
rows = ParamRows

-- | The @\'sort\'@ query parameter (ascending).
--
-- ==== __Examples__
--
-- >>> compile [sortAsc "foo"] [] ("foo" =: gt (int 5))
-- "sort=foo asc&q={!lucene}foo:{5 TO *]"
sortAsc :: Text -> Param a
sortAsc = ParamSortAsc

-- | The @\'sort\'@ query parameter (descending).
--
-- ==== __Examples__
--
-- >>> compile [sortDesc "foo"] [] ("foo" =: gt (int 5))
-- "sort=foo desc&q={!lucene}foo:{5 TO *]"
sortDesc :: Text -> Param a
sortDesc = ParamSortDesc

-- | The @\'start\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> compile [start 10] [] ("foo" =: word "bar")
-- "start=10&q={!lucene}foo:bar"
start :: Int -> Param a
start = ParamStart

module Solr.Query.Param where

import Solr.Prelude
import Solr.Query.Class
import Solr.Query.LocalParam

-- $setup
-- >>> import Solr.Query

-- | A query parameter.
data Param
  = ParamFl Text
  | ParamFq [LocalParam 'FilterQueryLocalParam] Query
  | ParamRows Int
  | ParamSortAsc Text
  | ParamSortDesc Text
  | ParamStart Int

-- | The @\'fl\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> compile [fl "baz", fl "qux"] [] ("foo" =: word "bar")
-- "fl=baz&fl=qux&q=foo:bar"
fl :: Text -> Param
fl = ParamFl

-- | The @\'fq\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> let filterQuery = "baz" =: gte (int 10)
-- >>> compile [fq [] filterQuery] [] ("foo" =: word "bar")
-- "fq=baz:[10 TO *]&q=foo:bar"
fq :: [LocalParam 'FilterQueryLocalParam] -> Query -> Param
fq = ParamFq

-- | The @\'rows\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> compile [rows 5] [] ("foo" =: word "bar")
-- "rows=5&q=foo:bar"
rows :: Int -> Param
rows = ParamRows

-- | The @\'sort\'@ query parameter (ascending).
--
-- ==== __Examples__
--
-- >>> compile [sortAsc "foo"] [] ("foo" =: gt (int 5))
-- "sort=foo asc&q=foo:{5 TO *]"
sortAsc :: Text -> Param
sortAsc = ParamSortAsc

-- | The @\'sort\'@ query parameter (descending).
--
-- ==== __Examples__
--
-- >>> compile [sortDesc "foo"] [] ("foo" =: gt (int 5))
-- "sort=foo desc&q=foo:{5 TO *]"
sortDesc :: Text -> Param
sortDesc = ParamSortDesc

-- | The @\'start\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> compile [start 10] [] ("foo" =: word "bar")
-- "start=10&q=foo:bar"
start :: Int -> Param
start = ParamStart

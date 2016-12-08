module Solr.Query.Param where

import Solr.Prelude
import Solr.Query.Class
import Solr.Query.LocalParam

-- $setup
-- >>> import Solr.Query

-- | A query parameter.
data Param
  = ParamFl Text
  | ParamFq FilterQueryLocalParams Query
  | ParamRows Int
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
fq :: FilterQueryLocalParams -> Query -> Param
fq = ParamFq

-- | The @\'rows\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> compile [rows 5] [] ("foo" =: word "bar")
-- "rows=5&q=foo:bar"
rows :: Int -> Param
rows = ParamRows

-- | The @\'start\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> compile [start 10] [] ("foo" =: word "bar")
-- "start=10&q=foo:bar"
start :: Int -> Param
start = ParamStart

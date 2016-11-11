module Solr.Param.Internal where

import Solr.Type

import Data.Text (Text)

-- $setup
-- >>> import Data.Semigroup
-- >>> import Solr.Query

data Param query where
  ParamCache        :: HasParamCache        query => Bool -> Param query
  ParamCost         :: HasParamCost         query => Int  -> Param query
  ParamDefaultField :: HasParamDefaultField query => Text -> Param query
  ParamOpAnd        :: HasParamOp           query =>         Param query
  ParamOpOr         :: HasParamOp           query =>         Param query
  ParamRows         :: HasParamRows         query => Int  -> Param query
  ParamStart        :: HasParamStart        query => Int  -> Param query

deriving instance Show (Param query)

-- | The @\'cache\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: word "bar" :: SolrFilterQuery SolrExpr
-- >>> compileSolrFilterQuery [paramCache False] query
-- "fq={!cache=false}foo:bar"
paramCache :: HasParamCache query => Bool -> Param query
paramCache = ParamCache

-- | The @\'cost\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: word "bar" :: SolrFilterQuery SolrExpr
-- >>> compileSolrFilterQuery [paramCost 5] query
-- "fq={!cost=5}foo:bar"
paramCost :: HasParamCost query => Int -> Param query
paramCost = ParamCost

-- | The @\'df\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = defaultField (word "bar") :: SolrQuery SolrExpr
-- >>> compileSolrQuery [paramDefaultField "foo"] query
-- "q={!df=foo}bar"
paramDefaultField :: HasParamDefaultField query => Text -> Param query
paramDefaultField = ParamDefaultField

-- | The @\'op=AND\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = defaultField (word "foo") <> defaultField (word "bar") :: SolrQuery SolrExpr
-- >>> compileSolrQuery [paramOpAnd] query
-- "q={!q.op=AND}foo bar"
paramOpAnd :: HasParamOp query => Param query
paramOpAnd = ParamOpAnd

-- | The @\'op=OR\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = defaultField (word "foo") <> defaultField (word "bar") :: SolrQuery SolrExpr
-- >>> compileSolrQuery [paramOpOr] query
-- "q={!q.op=OR}foo bar"
paramOpOr :: HasParamOp query => Param query
paramOpOr = ParamOpOr

-- | The @\'rows\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: word "bar" :: SolrQuery SolrExpr
-- >>> compileSolrQuery [paramRows 5] query
-- "q={!rows=5}foo:bar"
paramRows :: HasParamRows query => Int -> Param query
paramRows = ParamRows

-- | The @\'start\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: word "bar" :: SolrQuery SolrExpr
-- >>> compileSolrQuery [paramStart 10] query
-- "q={!start=10}foo:bar"
paramStart :: HasParamStart query => Int -> Param query
paramStart = ParamStart

class HasParamCache        (query :: (SolrType -> *) -> *)
class HasParamCost         (query :: (SolrType -> *) -> *)
class HasParamDefaultField (query :: (SolrType -> *) -> *)
class HasParamOp           (query :: (SolrType -> *) -> *)
class HasParamRows         (query :: (SolrType -> *) -> *)
class HasParamStart        (query :: (SolrType -> *) -> *)

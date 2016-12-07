module Solr.Param.Internal where

import Solr.Type

import Data.Text (Text)

-- $setup
-- >>> import Data.Semigroup
-- >>> import Solr.Query
-- >>> import Solr.FilterQuery (FilterQuery)
-- >>> import qualified Solr.FilterQuery as FilterQuery

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
-- >>> let query = "foo" =: word "bar" :: FilterQuery Expr
-- >>> FilterQuery.compile [cache False] query
-- "fq={!cache=false}foo:bar"
cache :: HasParamCache query => Bool -> Param query
cache = ParamCache

-- | The @\'cost\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: word "bar" :: FilterQuery Expr
-- >>> FilterQuery.compile [cost 5] query
-- "fq={!cost=5}foo:bar"
cost :: HasParamCost query => Int -> Param query
cost = ParamCost

-- | The @\'df\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = defaultField (word "bar") :: Query Expr
-- >>> compile [df "foo"] query
-- "q={!df=foo}bar"
df :: HasParamDefaultField query => Text -> Param query
df = ParamDefaultField

-- | The @\'op=AND\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = defaultField (word "foo") <> defaultField (word "bar") :: Query Expr
-- >>> compile [opAnd] query
-- "q={!q.op=AND}foo bar"
opAnd :: HasParamOp query => Param query
opAnd = ParamOpAnd

-- | The @\'op=OR\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = defaultField (word "foo") <> defaultField (word "bar") :: Query Expr
-- >>> compile [opOr] query
-- "q={!q.op=OR}foo bar"
opOr :: HasParamOp query => Param query
opOr = ParamOpOr

-- | The @\'rows\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: word "bar" :: Query Expr
-- >>> compile [rows 5] query
-- "q={!rows=5}foo:bar"
rows :: HasParamRows query => Int -> Param query
rows = ParamRows

-- | The @\'start\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: word "bar" :: Query Expr
-- >>> compile [start 10] query
-- "q={!start=10}foo:bar"
start :: HasParamStart query => Int -> Param query
start = ParamStart

class HasParamCache        (query :: (SolrType -> *) -> *)
class HasParamCost         (query :: (SolrType -> *) -> *)
class HasParamDefaultField (query :: (SolrType -> *) -> *)
class HasParamOp           (query :: (SolrType -> *) -> *)
class HasParamRows         (query :: (SolrType -> *) -> *)
class HasParamStart        (query :: (SolrType -> *) -> *)

module Solr.Query.Param.Internal where

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
  ParamStart        :: HasParamStart        query => Int  -> Param query

deriving instance Show (Param query)

-- | The @\'cache\'@ local parameter.
--
-- >>> params [paramCache False] ("foo" =: word "bar") :: SolrFilterQuery SolrExpr
-- fq={!cache=false}foo:bar
paramCache :: HasParamCache query => Bool -> Param query
paramCache = ParamCache

-- | The @\'cost\'@ local parameter.
--
-- >>> params [paramCost 5] ("foo" =: word "bar") :: SolrFilterQuery SolrExpr
-- fq={!cost=5}foo:bar
paramCost :: HasParamCost query => Int -> Param query
paramCost = ParamCost

-- | The @\'df\'@ local parameter.
--
-- >>> params [paramDefaultField "foo"] (defaultField (word "bar")) :: SolrQuery SolrExpr
-- q={!df=foo}bar
paramDefaultField :: HasParamDefaultField query => Text -> Param query
paramDefaultField = ParamDefaultField

-- | The @\'op=AND\'@ local parameter.
--
-- >>> params [paramOpAnd] (defaultField (word "foo") <> defaultField (word "bar")) :: SolrQuery SolrExpr
-- q={!q.op=AND}foo bar
paramOpAnd :: HasParamOp query => Param query
paramOpAnd = ParamOpAnd

-- | The @\'op=OR\'@ local parameter.
--
-- >>> params [paramOpOr] (defaultField (word "foo") <> defaultField (word "bar")) :: SolrQuery SolrExpr
-- q={!q.op=OR}foo bar
paramOpOr :: HasParamOp query => Param query
paramOpOr = ParamOpOr

-- | The @\'start\'@ local parameter.
--
-- >>> params [paramStart 10] ("foo" =: word "bar") :: SolrQuery SolrExpr
-- q={!q.start=10}foo:bar
paramStart :: HasParamStart query => Int -> Param query
paramStart = ParamStart

-- | The class of queries that support the @\'cache\'@ local parameter.
class HasParamCache (query :: (SolrType -> *) -> *)

-- | The class of queries that support the @\'cost\'@ local parameter.
class HasParamCost (query :: (SolrType -> *) -> *)

-- | The class of queries that support the @\'df\'@ local parameter.
class HasParamDefaultField (query :: (SolrType -> *) -> *)

-- | The class of queries that support the @\'op\'@ local parameter.
class HasParamOp (query :: (SolrType -> *) -> *)

-- | The class of queries that support the @\'start\'@ local parameter.
class HasParamStart (query :: (SolrType -> *) -> *)

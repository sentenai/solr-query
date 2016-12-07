module Solr.LocalParam.Internal where

import Solr.Type

import Data.Text (Text)

-- $setup
-- >>> import Data.Semigroup
-- >>> import Solr.Query
-- >>> import Solr.FilterQuery (FilterQuery)
-- >>> import qualified Solr.FilterQuery as FilterQuery

data LocalParam query where
  LocalParamCache :: HasLocalParamCache query => Bool -> LocalParam query
  LocalParamCost  :: HasLocalParamCost  query => Int  -> LocalParam query
  LocalParamDf    :: HasLocalParamDf    query => Text -> LocalParam query
  LocalParamOpAnd :: HasLocalParamOp    query =>         LocalParam query
  LocalParamOpOr  :: HasLocalParamOp    query =>         LocalParam query

deriving instance Show (LocalParam query)

-- | The @\'cache\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: word "bar" :: FilterQuery Expr
-- >>> FilterQuery.compile [] [cache False] query
-- "fq={!cache=false}foo:bar"
cache :: HasLocalParamCache query => Bool -> LocalParam query
cache = LocalParamCache

-- | The @\'cost\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: word "bar" :: FilterQuery Expr
-- >>> FilterQuery.compile [] [cost 5] query
-- "fq={!cost=5}foo:bar"
cost :: HasLocalParamCost query => Int -> LocalParam query
cost = LocalParamCost

-- | The @\'df\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = defaultField (word "bar") :: Query Expr
-- >>> compile [] [df "foo"] query
-- "q={!df=foo}bar"
df :: HasLocalParamDf query => Text -> LocalParam query
df = LocalParamDf

-- | The @\'op=AND\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = defaultField (word "foo") <> defaultField (word "bar") :: Query Expr
-- >>> compile [] [opAnd] query
-- "q={!q.op=AND}foo bar"
opAnd :: HasLocalParamOp query => LocalParam query
opAnd = LocalParamOpAnd

-- | The @\'op=OR\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let query = defaultField (word "foo") <> defaultField (word "bar") :: Query Expr
-- >>> compile [] [opOr] query
-- "q={!q.op=OR}foo bar"
opOr :: HasLocalParamOp query => LocalParam query
opOr = LocalParamOpOr

class HasLocalParamCache (query :: (SolrType -> *) -> *)
class HasLocalParamCost  (query :: (SolrType -> *) -> *)
class HasLocalParamDf    (query :: (SolrType -> *) -> *)
class HasLocalParamOp    (query :: (SolrType -> *) -> *)

module Solr.LocalParam.Internal where

import Solr.Type

import Data.Text (Text)

-- $setup
-- >>> import Data.Semigroup
-- >>> import Solr.Query

data LocalParam (query :: (SolrType -> *) -> *)
  = LocalParamCache Bool
  | LocalParamCost  Int
  | LocalParamDf    Text
  | LocalParamOpAnd
  | LocalParamOpOr

deriving instance Show (LocalParam query)

-- | The @\'cache\'@ local parameter.
--
cache :: HasLocalParamCache query => Bool -> LocalParam query
cache = LocalParamCache

-- | The @\'cost\'@ local parameter.
--
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

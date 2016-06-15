module Solr.Param where

import Solr.Class

import Data.Text (Text)

-- $setup
-- >>> import Data.Semigroup
-- >>> import Solr.Query

-- | The class of queries that support the @\'df\'@ local parameter.
class HasParamDefaultField query where
  -- | The @\'df\'@ local parameter.
  --
  -- >>> params [paramDefaultField .= "foo"] (defaultField (word "bar")) :: SolrQuery
  -- q={!df=foo}bar
  paramDefaultField :: ParamKey query Text

-- | The class of queries that support the @\'op\'@ local parameter.
class HasParamOp query where
  -- | The @\'op\'@ local parameter.
  --
  -- Stringly typed to avoid a clunky sum type like
  --
  -- @
  -- data Operator = And | Or | ...
  -- @
  --
  -- which seems to have little value in cases like this. Instead, just pass
  -- @\"AND\"@, @\"OR\"@, ...
  --
  -- >>> params [paramOp .= "AND"] (defaultField (word "foo") <> defaultField (word "bar")) :: SolrQuery
  -- q={!q.op=AND}foo bar
  paramOp :: ParamKey query Text

-- | The class of queries that support the @\'cache\'@ local parameter.
class HasParamCache query where
  -- | The @\'cache\'@ local parameter.
  --
  -- >>> params [paramCache .= False] ("foo" =: word "bar") :: SolrFilterQuery
  -- fq={!cache=false}foo:bar
  paramCache :: ParamKey query Bool

-- | The class of queries that support the @\'cost\'@ local parameter.
class HasParamCost query where
  -- | The @\'cost\'@ local parameter.
  --
  -- >>> params [paramCost .= 5] ("foo" =: word "bar") :: SolrFilterQuery
  -- fq={!cost=5}foo:bar
  paramCost :: ParamKey query Int

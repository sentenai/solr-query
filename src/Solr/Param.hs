module Solr.Param where

import Solr.Class

import Data.Text (Text)

-- | The class of queries that support the @\'df\'@ local parameter.
class HasParamDefaultField query where
  -- | The @\'df\'@ local parameter.
  --
  -- Example:
  --
  -- @
  -- -- {!df=foo}bar
  -- query :: 'Solr.Query.SolrQuery'
  -- query = 'Solr.Query.params' ['Solr.Query.paramDefaultField' 'Solr.Query..=' "foo"] ('Solr.Query.defaultField' ('Solr.Query.word' "bar"))
  -- @
  paramDefaultField :: ParamKey query Text

-- | The class of queries that support the @\'op\'@ local parameter.
class HasParamOp query where
  -- | The @\'op\'@ local parameter.
  --
  -- Stringly typed to avoid a clunky sum type like
  --
  -- @
  -- data Val = And | Or | ...
  -- @
  --
  -- which seems to have little value in cases like this. Instead, just pass
  -- @\"AND\"@, @\"OR\"@, ...
  --
  -- Example:
  --
  -- @
  -- -- {!q.op=AND}foo bar
  -- query :: 'Solr.Query.SolrQuery'
  -- query = 'Solr.Query.params' ['Solr.Query.paramOp' 'Solr.Query..=' \"AND\"] ('Solr.Query.defaultField' ('Solr.Query.word' "foo") 'Data.Semigroup.<>' 'Solr.Query.defaultField' ('Solr.Query.word' "bar"))
  -- @
  paramOp :: ParamKey query Text

-- | The class of queries that support the @\'cache\'@ local parameter.
class HasParamCache query where
  -- | The @\'cache\'@ local parameter.
  --
  -- Example:
  --
  -- @
  -- -- {!cache=false}foo:bar
  -- query :: 'Solr.Query.SolrFilterQuery'
  -- query = 'Solr.Query.params' ['Solr.Query.paramCache' 'Solr.Query..=' False] ("foo" 'Solr.Query.=:' 'Solr.Query.word' "bar")
  -- @
  paramCache :: ParamKey query Bool

-- | The class of queries that support the @\'cost\'@ local parameter.
class HasParamCost query where
  -- | The @\'cost\'@ local parameter.
  --
  -- Example:
  --
  -- @
  -- -- {!cost=5}foo:bar
  -- query :: 'Solr.Query.SolrFilterQuery'
  -- query = 'Solr.Query.params' ['Solr.Query.paramCost' 'Solr.Query..=' 5] ("foo" 'Solr.Query.=:' 'Solr.Query.word' "bar")
  -- @
  paramCost :: ParamKey query Int

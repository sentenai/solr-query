module Solr.Query.Filter.Internal where

import Solr.Prelude

import Builder
import Solr.Query.Internal

compileFilterQuery :: Query query => FilterParams query -> query -> Builder
compileFilterQuery locals query =
  case compileFilterParams locals of
    [] -> coerce query
    xs -> "{!" <> intersperse ' ' xs <> char '}' <> coerce query

compileFilterParams :: Query query => FilterParams query -> [Builder]
compileFilterParams FilterParams{_cache, _cost, _locals} = catMaybes
  [ compileCache <$> _cache
  , compileCost <$> _cost
  ] ++ maybe [] compileLocalParams _locals
 where
  compileCache :: Bool -> Builder
  compileCache True  = "cache=true"
  compileCache False = "cache=false"

  compileCost :: Int -> Builder
  compileCost n = "cost=" <> bshow n

data FilterParams query = FilterParams
  { _cache :: Maybe Bool
  , _cost :: Maybe Int
  , _locals :: Maybe (LocalParams query)
  }

instance Default (FilterParams query) where
  def :: FilterParams query
  def = FilterParams Nothing Nothing Nothing

-- | The @\'df\'@ local parameter.
cache :: Bool -> FilterParams query -> FilterParams query
cache x s = s { _cache = Just x }

-- | The @\'df\'@ local parameter.
cost :: Int -> FilterParams query -> FilterParams query
cost x s = s { _cost = Just x }

-- | 'LocalParams' of the inner 'Query'.
locals :: LocalParams query -> FilterParams query -> FilterParams query
locals x s = s { _locals = Just x }

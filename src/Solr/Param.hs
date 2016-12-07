-- | Query parameters.

module Solr.Param
  ( -- * Query parameters
    Param(..)
  , rows
  , start
  ) where

import Solr.Type

-- $setup
-- >>> import Data.Semigroup
-- >>> import Solr.Query

data Param
  = ParamRows Int
  | ParamStart Int
  deriving (Eq, Show)

-- | The @\'rows\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: word "bar" :: Query Expr
-- >>> compile [rows 5] []  query
-- "rows=5&q=foo:bar"
rows :: Int -> Param
rows = ParamRows

-- | The @\'start\'@ query parameter.
--
-- ==== __Examples__
--
-- >>> let query = "foo" =: word "bar" :: Query Expr
-- >>> compile [start 10] query
-- "start=10&q=foo:bar"
start :: Int -> Param
start = ParamStart

{-# LANGUAGE DataKinds #-}

-- | This module is an alternative to "Solr.Class" that does not export any
-- operators, and is intended to be imported qualified, because it contains
-- function names that clash with the Prelude.
--
-- > import qualified Solr.Qualified.Class as Solr
--
-- Here is a quick conversion guide:
--
-- @
-- ('Solr.Class.~:')  = 'fuzz'
-- ('Solr.Class.^:')  = 'boost'
-- ('Solr.Class.=:')  = 'field'
-- ('Solr.Class.&&:') = 'and'
-- ('Solr.Class.||:') = 'or'
-- ('Solr.Class.-:')  = 'not'
-- ('Solr.Class.^=:') = 'score'
-- @

module Solr.Qualified.Class
  (
    -- * Named operators
    fuzz
  , boost
  , field
  , and
  , or
  , not
  , score
  -- * Re-exports
  , module Solr.Class
  ) where

import Solr.Class hiding ((~:), (^:), (=:), (&&:), (||:), (-:), (^=:))
import Solr.Type

import qualified Solr.Class as Solr

import Data.Text (Text)
import Prelude   hiding (and, not, or)


-- | Named version of ('Solr.Class.~:').
fuzz :: (SolrExprSYM expr, FuzzableType a) => expr a -> Int -> expr (TFuzzed a)
fuzz = (Solr.~:)

-- | Named version of ('Solr.Class.^:').
boost :: (SolrExprSYM expr, BoostableType a) => expr a -> Float -> expr (TBoosted a)
boost = (Solr.^:)

-- | Named version of ('Solr.Class.=:').
field :: SolrQuerySYM expr query => Text -> expr a -> query 'False 'False
field = (Solr.=:)

-- | Named version of ('Solr.Class.&&:').
and :: SolrQuerySYM expr query => query 'False 'False -> query 'False 'False -> query 'False 'False
and = (Solr.&&:)
infixr 3 `and`

-- | Named version of ('Solr.Class.||:').
or :: SolrQuerySYM expr query => query 'False 'False -> query 'False 'False -> query 'False 'False
or = (Solr.||:)
infixr 2 `or`

-- | Named version of ('Solr.Class.-:').
not :: SolrQuerySYM expr query => query 'False 'False -> query 'False 'False -> query 'False 'False
not = (Solr.-:)
infixr 1 `not`

-- | Named version of ('Solr.Class.^=:').
score :: SolrQuerySYM expr query => query 'False 'False -> Float -> query 'False 'False
score = (Solr.^=:)
infixr 4 `score`

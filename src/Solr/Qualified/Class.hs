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
    -- * Solr language
    SolrExprSYM
  , int
  , true
  , false
  , word
  , wild
  , regex
  , phrase
  , fuzz
  , to
  , boost
  , SolrQuerySYM
  , defaultField
  , field
  , and
  , or
  , not
  , score
  , neg
  , localParams
    -- * Derived combinators
  , fuzzy
  , gt
  , gte
  , lt
  , lte
    -- * Range query helpers
  , Boundary(..)
  , incl
  , excl
  , star
  ) where

import Solr.Type
import Solr.Class

import Data.Text (Text)
import Prelude   hiding (and, not, or)

-- | Named version of ('~:').
fuzz :: (SolrExprSYM expr, FuzzableType a) => expr a -> Int -> expr (TFuzzed a)
fuzz = (~:)

-- | Named version of ('^:').
boost :: (SolrExprSYM expr, BoostableType a) => expr a -> Float -> expr (TBoosted a)
boost = (^:)

-- | Named version of ('=:').
field :: SolrQuerySYM expr query => Text -> expr a -> query 'False 'False
field = (=:)

-- | Named version of ('&&:').
and :: SolrQuerySYM expr query => query 'False 'False -> query 'False 'False -> query 'False 'False
and = (&&:)
infixr 3 `and`

-- | Named version of ('||:').
or :: SolrQuerySYM expr query => query 'False 'False -> query 'False 'False -> query 'False 'False
or = (||:)
infixr 2 `or`

-- | Named version of ('-:').
not :: SolrQuerySYM expr query => query 'False 'False -> query 'False 'False -> query 'False 'False
not = (-:)
infixr 1 `not`

-- | Named version of ('^=:').
score :: SolrQuerySYM expr query => query 'False 'False -> Float -> query 'False 'False
score = (^=:)
infixr 4 `score`

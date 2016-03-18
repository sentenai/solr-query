-- | This module is an alternative to "Solr.Query" that does not export any
-- operators, and is intended to be imported qualified, because it contains
-- function names that clash with the Prelude.
--
-- > import qualified Solr.Query.Qualified as Solr
--
-- Here is a quick conversion guide:
--
-- @
-- ('~:')  = 'fuzz'
-- ('^:')  = 'boost'
-- ('=:')  = 'field'
-- ('&&:') = 'and'
-- ('||:') = 'or'
-- ('-:')  = 'not'
-- ('^=:') = 'score'
-- @

module Solr.Query.Qualified
  (
  -- * Query type
    SolrQuery
  -- * Query construction
  -- $note-simplicity
  , defaultField
  , field
  , and
  , or
  , not
  , score
  -- * Expression type
  , SolrExpr
  -- * Expression construction
  -- $note-simplicity
  , int
  , true
  , false
  , word
  , wild
  , regex
  , phrase
  , fuzz
  , fuzzy
  , to
  , gt
  , gte
  , lt
  , lte
  , boost
  -- * Query compilation
  , compileSolrQuery
  ) where

import Solr.Class.Qualified
import Solr.Query (SolrExpr, SolrQuery, compileSolrQuery)

import Prelude hiding (and, not, or)

-- $note-simplicity
-- For simplicity, the type signatures in the examples below monomorphise the
-- functions to use 'SolrQuery' (and therefore 'SolrExpr', due to the
-- functional dependency).

-- | This module is an alternative to "Solr.Query" that does not export any
-- operators, and is intended to be imported qualified, because it contains
-- function names that clash with the Prelude.
--
-- > import qualified Solr.Qualified.Query as Solr
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

module Solr.Qualified.Query
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
  , neg
  , localParams
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
  -- * Local parameters
  --
  -- | Combine 'LocalParams' 'SolrQuery' with ('<>') and mark a query as having
  -- local parameters with 'localParams'.
  , paramDefaultField
  , paramOp
  -- * Query compilation
  , compileSolrQuery
  ) where

import Solr.Qualified.Class
import Solr.Query

import Prelude hiding (and, not, or)

-- $note-simplicity
-- For simplicity, the type signatures in the examples below monomorphise the
-- functions to use 'SolrQuery' (and therefore 'SolrExpr', due to the
-- functional dependency).

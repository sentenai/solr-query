-- | This module is an alternative to "Lucene.Query" that does not export any
-- operators, and is intended to be imported qualified, because it contains
-- function names that clash with the Prelude.
--
-- > import qualified Lucene.Query.Qualified as Lucene
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

module Lucene.Query.Qualified
  (
  -- * Query type
    LuceneQuery
  -- * Query construction
  -- $note-simplicity
  , field
  , and
  , or
  , not
  , score
  -- * Expression type
  , LuceneExpr
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
  , compileLuceneQuery
  ) where

import Lucene.Class.Qualified
import Lucene.Query (LuceneExpr, LuceneQuery, compileLuceneQuery)

import Prelude hiding (and, not, or)

-- $note-simplicity
-- For simplicity, the type signatures in the examples below monomorphise the
-- functions to use 'LuceneQuery' (and therefore 'LuceneExpr', due to the
-- functional dependency).

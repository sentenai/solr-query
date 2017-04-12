module Solr.Query.Lucene
  ( -- * Lucene query
    LuceneQuery
  , neg
  , defaultField
  , (=:)
  , field
    -- ** Lucene expression
  , LuceneExpr
    -- *** @int@ expression
  , int
    -- *** @float@ expression
  , float
    -- *** @bool@ expressions
  , true
  , false
    -- *** @word@ expression
  , word
  , fuzzy
    -- *** @wild@ expression
  , wild
    -- *** @regex@ expression
  , regex
    -- *** @phrase@ expression
  , phrase
  , proximity
    -- *** @datetime@ expression
  , datetime
  , DateTime
  , Year
  , Month
  , Day
  , Hour
  , Minute
  , Second
  , Millisecond
    -- *** @range@ expressions
  , to
  , gt
  , gte
  , lt
  , lte
  , Boundary
  , incl
  , excl
  , star
    -- *** @spatial predicate@ expressions
  , intersects
  , isWithin
  , Shape
  , polygon
    -- *** Embedded 'GeofiltQuery'
  , geofilt
    -- *** Lucene expression types
  , LuceneExprTy(..)
  , Boostable
  , Rangeable
    -- ** Local parameters
  , df
  , opAnd
  , opOr
    -- * Re-exports
  , UTCTime
  ) where

import Solr.Query.Lucene.Expr
import Solr.Query.Lucene.Expr.Type
import Solr.Query.Lucene.Internal.Internal

import Data.Time.Clock (UTCTime)

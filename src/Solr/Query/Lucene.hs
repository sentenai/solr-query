module Solr.Query.Lucene
  ( -- * Interpreting a query
    compile
  , compileParam
  , compileLocalParam
  , compileQuery
    -- * Lucene query language
  , LuceneQuery
  , LuceneQuerySYM(..)
  , neg
    -- ** Named operators
  , field
  , qand
  , qor
  , qnot
  , score
    -- * Solr expression language
  , ExprSYM(..)
    -- ** Named operators
  , fuzz
  , boost
    -- ** Derived operators
  , fuzzy
  , gt
  , gte
  , lt
  , lte
    -- ** Range expression
  , Boundary(..)
  , incl
  , excl
  , star
    -- ** Datetime expression
  , DateTime
  , IsDateTime
  , Year
  , Month
  , Day
  , Hour
  , Minute
  , Second
  , Millisecond
    -- ** Solr expression types
  , ExprTy(..)
  , Fuzzable
  , Boostable
  , Rangeable
    -- * Query parameters
  , Param
  , Cache
  , Cost
  , fl
  , fq
  , rows
  , sortAsc
  , sortDesc
  , start
    -- * Local query parameters
  , LuceneQueryParam
  , df
  , opAnd
  , opOr
    -- * Re-exports
  , UTCTime
  ) where

import Solr.Expr.Class
import Solr.Expr.Type
import Solr.Query.Lucene.Class
import Solr.Query.Lucene.Impl.Final
  (compile, compileLocalParam, compileParam, compileQuery)
import Solr.Query.Param

import Data.Time (UTCTime)

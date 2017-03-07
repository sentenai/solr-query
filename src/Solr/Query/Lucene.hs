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
    -- * Lucene expression language
  , LuceneExprSYM(..)
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
    -- ** Lucene expression types
  , LuceneExprTy(..)
  , Fuzzable
  , Boostable
  , Rangeable
    -- * Local query parameters
  , LuceneQueryParam
  , df
  , opAnd
  , opOr
    -- * Re-exports
  , UTCTime
  ) where

import Solr.Query.Lucene.Class
import Solr.Query.Lucene.Expr.Class
import Solr.Query.Lucene.Expr.Type
import Solr.Query.Lucene.Impl.Final (compile, compileLocalParam, compileQuery)
import Solr.Query.Utils (compileParam)

import Data.Time (UTCTime)

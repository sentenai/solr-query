module Solr.Query
  ( -- * Interpreting a query
    compile
    -- * Solr query language
  , Query
  , QuerySYM(..)
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
  , fl
  , fq
  , rows
  , start
    -- * Local query parameters
  , LocalParam
  , LocalParamTy(..)
  , cache
  , cost
  , df
  , opAnd
  , opOr
    -- * Re-exports
  , UTCTime
  ) where

import Solr.Expr.Class
import Solr.Expr.Type
import Solr.Query.Class
import Solr.Query.Impl.Final (compile)
import Solr.Query.LocalParam
import Solr.Query.Param

import Data.Time (UTCTime)

module Solr.Query.Initial
  ( -- * Interpreting a query
    compile
  , typeCheck
  , factor
  , reinterpret
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
  , DateTime(..)
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
  , Param(..)
  , fl
  , fq
  , rows
  , start
    -- * Local query parameters
  , LocalParam(..)
  , QueryLocalParams
  , FilterQueryLocalParams
  , cache
  , cost
  , df
  , opAnd
  , opOr
  , HasLocalParamCache
  , HasLocalParamCost
  , HasLocalParamDf
  , HasLocalParamOp
    -- * Initially-encoded query
  , Q(..)
    -- ** Untyped expression
  , UExpr(..)
    -- ** Typed expression
  , Expr(..)
    -- * Re-exports
  , UTCTime
  ) where

import Solr.Expr.Class
import Solr.Expr.Impl.Initial.Untyped (UExpr(..))
import Solr.Expr.Impl.Initial.Typed (Expr(..))
import Solr.Expr.Type
import Solr.Query.Class
import Solr.Query.Impl.Final (compile)
import Solr.Query.Impl.Initial (Q(..), factor, reinterpret, typeCheck)
import Solr.Query.LocalParam
import Solr.Query.Param

import Data.Time (UTCTime)

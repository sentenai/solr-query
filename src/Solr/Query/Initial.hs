module Solr.Query.Initial
  ( -- * Manipulating a query
    typeCheck
  , factor
  , reinterpret
    -- * Initially-encoded query
  , Q(..)
    -- ** Untyped expression
  , UExpr(..)
    -- ** Typed expression
  , Expr(..)
    -- * Misc. internals
  , Boundary(..)
  , DateTime(..)
  , Param(..)
  , LocalParam(..)
    -- * Re-exports
  , module Solr.Query
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
import Solr.Query

import Data.Time (UTCTime)

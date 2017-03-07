module Solr.Query.Lucene.Initial
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
  , LuceneExprTy(..)
    -- * Misc. internals
  , Boundary(..)
  , DateTime(..)
  , Param(..)
  , LocalParam(..)
    -- * Re-exports
  , module Solr.Query.Lucene
  ) where

import Solr.Query.Lucene
import Solr.Query.Lucene.Class
import Solr.Query.Lucene.Expr.Class
import Solr.Query.Lucene.Expr.Impl.Initial.Untyped (UExpr(..))
import Solr.Query.Lucene.Expr.Impl.Initial.Typed (Expr(..))
import Solr.Query.Lucene.Impl.Final (compile)
import Solr.Query.Lucene.Impl.Initial (Q(..), factor, reinterpret, typeCheck)
import Solr.Query.Param

import Data.Time (UTCTime)

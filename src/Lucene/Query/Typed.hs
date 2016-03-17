{-# LANGUAGE ExistentialQuantification #-}

module Lucene.Query.Typed where

import Lucene.Expr.Typed

data LuceneQuery
  = forall a. QExpr (LuceneExpr a)

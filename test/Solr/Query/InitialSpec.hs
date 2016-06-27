{-# LANGUAGE ScopedTypeVariables #-}

module Solr.Query.InitialSpec where

import Orphans
import Solr.Query.Initial
import Solr.Expr.Initial.Untyped

import Data.Generics.Uniplate
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "factorSolrQuery" $
    prop "pushes all params to the top" $ \(q :: SolrQuery SolrExpr) ->
      case factorSolrQuery q of
        QParams _ q' -> null (filter isParams (universe q'))
         where
          isParams (QParams _ _) = True
          isParams _ = False

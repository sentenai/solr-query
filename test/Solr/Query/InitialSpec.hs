module Solr.Query.InitialSpec where

import Orphans ()
import Solr.Query.Initial
import Solr.Expr.Initial.Untyped

import Test.Hspec

spec :: Spec
spec = do
  describe "factorSolrQuery" $
    it "eliminates double negations" $ do
      let q = QField "foo" ETrue
      factorSolrQuery (QNeg (QNeg q)) `shouldBe` q

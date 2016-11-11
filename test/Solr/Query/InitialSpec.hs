module Solr.Query.InitialSpec where

import Orphans ()
import Solr.Query.Initial
import Solr.Expr.Initial.Untyped

import Test.Hspec

spec :: Spec
spec = do
  describe "factor" $
    it "eliminates double negations" $ do
      let q = QField "foo" ETrue
      factor (QNeg (QNeg q)) `shouldBe` q

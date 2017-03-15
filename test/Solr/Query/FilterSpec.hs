module Solr.Query.FilterSpec where

import Solr.Query
import Solr.Query.Filter
import Solr.Query.Geofilt
import Solr.Query.Lucene

import Data.Text.Lazy (Text)
import Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "compile" $ do
    describe "params" $ do
      it "cache" (test (def & cache True) ("foo" =: word "bar") "fq={!cache=true}foo:bar")
      it "cost" (test (def & cost 5) ("foo" =: word "bar") "fq={!cost=5}foo:bar")
      it "locals" $ do
        test (def & locals def) ("foo" =: word "bar") "fq=foo:bar"
        test (def & locals (def & (d 5.5))) def "fq={!geofilt d=5.5}"

test :: Query query => FilterParams query -> query -> Text -> Expectation
test locals query result = compile [fq locals query] `shouldBe` result

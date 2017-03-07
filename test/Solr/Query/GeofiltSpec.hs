module Solr.Query.GeofiltSpec where

import Solr.Query
import Solr.Query.Geofilt
import Solr.Query.Lucene (defaultField, word)

import qualified Solr.Query.Lucene as Lucene

import Data.Text.Lazy (Text)
import Prelude.Compat
import Test.Hspec

spec :: Spec
spec = do
  describe "compile" $ do
    describe "params" $ do
      it "d" (test [d 1.5] "q={!geofilt d=1.5}")
      it "pt" (test [pt 1.5 2.5] "q={!geofilt pt=1.5,2.5}")
      it "sfield" (test [sfield "foo"] "q={!geofilt sfield=foo}")

  it "inside a filter query" $ do
    Lucene.compile [fq DontCache Nothing [d 1.5] geofilt] [] (defaultField (word "foo"))
      `shouldBe` "fq={!geofilt d=1.5 cache=false}&q={!lucene}foo"

test :: [GeofiltQueryParam] -> Text -> Expectation
test locals result = compile [] locals `shouldBe` result

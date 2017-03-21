module Solr.Query.GeofiltSpec where

import Solr.Query
import Solr.Query.Geofilt

import Data.Text.Lazy (Text)
import Prelude.Compat
import Test.Hspec

spec :: Spec
spec = do
  describe "compile" $ do
    describe "params" $ do
      it "d" (test (def & d 1.5) "q={!type=geofilt d=1.5}")
      it "pt" (test (def & pt 1.5 2.5) "q={!type=geofilt pt=1.5,2.5}")
      it "sfield" (test (def & sfield "foo") "q={!type=geofilt sfield=foo}")

test :: LocalParams GeofiltQuery -> Text -> Expectation
test locals result = compile [] locals def `shouldBe` result

module Solr.Query.InitialSpec where

import Orphans ()
import Solr.Query.Initial

import Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "factor" $
    it "eliminates double negations" $ do
      let q = QField "foo" ETrue

      compile [] []
        (reinterpret (factor
          (QNot (QField "*" (ETo Star Star))
            (QNot (QField "*" (ETo Star Star)) q))))
        `shouldBe` "q=foo:true"

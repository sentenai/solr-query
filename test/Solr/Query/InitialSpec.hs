module Solr.Query.InitialSpec where

import Orphans ()
import Solr.Query.Initial
import Solr.Expr.Initial.Typed (Expr(..))

import qualified Solr.Query as Final

import Data.Text.Lazy (Text)
import Test.Hspec

spec :: Spec
spec = do
  describe "factor" $
    it "eliminates double negations" $ do
      let q = QField "foo" ETrue

      compile (reinterpret (factor
        (QNot (QField "*" (ETo Star Star))
          (QNot (QField "*" (ETo Star Star)) q))))
        `shouldBe` "q=foo:true"

compile :: Final.Query Final.Expr -> Text
compile = Final.compile [] []

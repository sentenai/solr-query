{-# LANGUAGE OverloadedStrings #-}

import Solr.Query

import Data.ByteString.Lazy (ByteString)
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "compileSolrQuery" $ do
    test (defaultField (word "foo"))                   "foo"
    test ("foo" =: word "bar")                         "foo:bar"
    test ("foo" =: word "bar" &&: "baz" =: word "qux") "(foo:bar AND baz:qux)"
    test ("foo" =: word "bar" ||: "baz" =: word "qux") "(foo:bar OR baz:qux)"
    test ("foo" =: word "bar" -: "baz" =: word "qux")  "(foo:bar NOT baz:qux)"
    test ("foo" =: word "bar" ^=: 3.5)                 "(foo:bar)^=3.5"
    test (neg ("foo" =: word "bar"))                   "-foo:bar"
    test (params ps1 (defaultField (word "bar")))      "{!df=foo}bar"
    test (params ps2 (defaultField (word "bar")))      "{!df=foo q.op=FOO}bar"
 where
  ps1 = [paramDefaultField .= "foo"]
  ps2 = [paramDefaultField .= "foo", paramOp .= "FOO"]


test :: SolrQuery -> ByteString -> Expectation
test query result = compileSolrQuery query `shouldBe` result

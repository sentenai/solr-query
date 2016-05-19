{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

import Solr.Class
import Solr.Query
import Solr.Type

import Data.ByteString.Lazy (ByteString)
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "compileSolrQuery" $ do
    it "defaultField"    (test (defaultField (word "foo"))                   "q=foo")
    it "=:"              (test ("foo" =: word "bar")                         "q=foo:bar")
    it "num"             (test ("foo" =: num 5)                              "q=foo:5.0")
    it "true"            (test ("foo" =: true)                               "q=foo:true")
    it "false"           (test ("foo" =: false)                              "q=foo:false")
    it "wild"            (test ("foo" =: wild "b?r")                         "q=foo:b?r")
    it "regex"           (test ("foo" =: regex "[mb]oat")                    "q=foo:/[mb]oat/")
    it "phrase"          (test ("foo" =: phrase ["bar", "baz"])              "q=foo:\"bar baz\"")
    it "word ~:"         (test ("foo" =: word "bar" ~: 1)                    "q=foo:bar~1")
    it "phrase ~:"       (test ("foo" =: phrase ["bar", "baz"] ~: 1)         "q=foo:\"bar baz\"~1")
    it "incl incl range" (test ("foo" =: incl (num 5) `to` incl (num 6))     "q=foo:[5.0 TO 6.0]")
    it "excl incl range" (test ("foo" =: excl (num 5) `to` incl (num 6))     "q=foo:{5.0 TO 6.0]")
    it "incl excl range" (test ("foo" =: incl (num 5) `to` excl (num 6))     "q=foo:[5.0 TO 6.0}")
    it "excl excl range" (test ("foo" =: excl (num 5) `to` excl (num 6))     "q=foo:{5.0 TO 6.0}")
    it "star incl range" (test ("foo" =: star `to` incl (num 5))             "q=foo:[* TO 5.0]")
    it "incl star range" (test ("foo" =: incl (num 5) `to` star)             "q=foo:[5.0 TO *]")
    it "star star range" (test ("foo" =: star `to` star')                    "q=foo:[* TO *]")
    it "gt"              (test ("foo" =: gt (num 5))                         "q=foo:{5.0 TO *]")
    it "gte"             (test ("foo" =: gte (num 5))                        "q=foo:[5.0 TO *]")
    it "lt"              (test ("foo" =: lt (num 5))                         "q=foo:[* TO 5.0}")
    it "lte"             (test ("foo" =: lte (num 5))                        "q=foo:[* TO 5.0]")
    it "word ^:"         (test ("foo" =: word "bar" ^: 3)                    "q=foo:bar^3.0")
    it "phrase ^:"       (test ("foo" =: phrase ["bar", "baz"] ^: 3)         "q=foo:\"bar baz\"^3.0")
    it "&&:"             (test ("foo" =: word "bar" &&: "baz" =: word "qux") "q=(foo:bar AND baz:qux)")
    it "&&: mempty"      (test ("foo" =: word "bar" &&: mempty)              "q=(foo:bar AND )")
    it "||:"             (test ("foo" =: word "bar" ||: "baz" =: word "qux") "q=(foo:bar OR baz:qux)")
    it "-:"              (test ("foo" =: word "bar" -: "baz" =: word "qux")  "q=(foo:bar NOT baz:qux)")
    it "^=:"             (test ("foo" =: word "bar" ^=: 3.5)                 "q=(foo:bar)^=3.5")
    it "neg"             (test (neg ("foo" =: word "bar"))                   "q=-foo:bar")
    it "zero params"     (test (params [] (defaultField (word "bar")))       "q={!}bar")
    it "one param"       (test (params ps1 (defaultField (word "bar")))      "q={!df=foo}bar")
    it "two params"      (test (params ps2 (defaultField (word "bar")))      "q={!df=foo q.op=FOO}bar")
 where
  ps1 = [paramDefaultField .= "foo"]
  ps2 = [paramDefaultField .= "foo", paramOp .= "FOO"]

  -- Help type inference in [* TO *]
  star' :: Boundary (SolrExpr 'TNum)
  star' = star


test :: SolrQuery -> ByteString -> Expectation
test query result = compileSolrQuery query `shouldBe` result

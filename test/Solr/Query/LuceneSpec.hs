module Solr.Query.LuceneSpec where

import Solr.Query
import Solr.Query.Lucene

import Data.Text.Lazy (Text)
import Data.Time
import Prelude.Compat
import Test.Hspec

spec :: Spec
spec =
  describe "compile" $ do
    it "defaultField" (test def (defaultField (word "foo")) "q=foo")
    it "field" (test def ("foo" =: word "bar") "q=foo:bar")
    it "int" (test def ("foo" =: int 5) "q=foo:5")
    it "float" (test def ("foo" =: float 5) "q=foo:5.0")
    it "true" (test def ("foo" =: true) "q=foo:true")
    it "false" (test def ("foo" =: false) "q=foo:false")
    it "wild" (test def ("foo" =: wild "b?r") "q=foo:b?r")
    it "regex" (test def ("foo" =: regex "[mb]oat") "q=foo:/[mb]oat/")
    it "phrase" (test def ("foo" =: phrase ["bar", "baz"]) "q=foo:\"bar baz\"")

    describe "datetime" $ do
      it "utc" (test def ("foo" =: datetime t1) "q=foo:\"2015-01-01T00:00:00Z\"")
      it "Y" (test def ("foo" =: datetime (2015::Year)) "q=foo:\"2015\"")
      it "YM" (test def ("foo" =: datetime (2015, 1)) "q=foo:\"2015-01\"")
      it "YMD" (test def ("foo" =: datetime (2015, 1, 2)) "q=foo:\"2015-01-02\"")
      it "YMD:H" (test def ("foo" =: datetime (2015, 1, 2, 3)) "q=foo:\"2015-01-02T03\"")
      it "YMD:HM" (test def ("foo" =: datetime (2015, 1, 2, 3, 4)) "q=foo:\"2015-01-02T03:04\"")
      it "YMD:HMS" (test def ("foo" =: datetime (2015, 1, 2, 3, 4, 5)) "q=foo:\"2015-01-02T03:04:05\"")
      it "YMD:HMSs" (test def ("foo" =: datetime (2015, 1, 2, 3, 4, 5, 6)) "q=foo:\"2015-01-02T03:04:05.06000Z\"")
      it "clamps month -> 1" (test def ("foo" =: datetime (2015, 0)) "q=foo:\"2015-01\"")
      it "clamps month 12 <-" (test def ("foo" =: datetime (2015, 13)) "q=foo:\"2015-12\"")
      it "clamps day -> 1" (test def ("foo" =: datetime (2015, 1, 0)) "q=foo:\"2015-01-01\"")
      it "clamps day 31 <-" (test def ("foo" =: datetime (2015, 1, 32)) "q=foo:\"2015-01-31\"")
      it "clamps hour -> 0" (test def ("foo" =: datetime (2015, 1, 1, -1)) "q=foo:\"2015-01-01T00\"")
      it "clamps hour 23 <-" (test def ("foo" =: datetime (2015, 1, 1, 24)) "q=foo:\"2015-01-01T23\"")
      it "clamps minute -> 0" (test def ("foo" =: datetime (2015, 1, 1, 1, -1))        "q=foo:\"2015-01-01T01:00\"")
      it "clamps minute 59 <-" (test def ("foo" =: datetime (2015, 1, 1, 1, 60)) "q=foo:\"2015-01-01T01:59\"")
      it "clamps second -> 0" (test def ("foo" =: datetime (2015, 1, 1, 1, 1, -1)) "q=foo:\"2015-01-01T01:01:00\"")
      it "clamps second 60 <-" (test def ("foo" =: datetime (2015, 1, 1, 1, 1, 61)) "q=foo:\"2015-01-01T01:01:60\"")
      it "clamps millisecond -> 0" (test def ("foo" =: datetime (2015, 1, 1, 1, 1, 1, -1)) "q=foo:\"2015-01-01T01:01:01.00000Z\"")
      it "clamps millisecond 99.999 <-" (test def ("foo" =: datetime (2015, 1, 1, 1, 1, 1, 100)) "q=foo:\"2015-01-01T01:01:01.99999Z\"")

    describe "fuzzy" $ do
      it "word" (test def ("foo" =: word "bar" ~: 1) "q=foo:bar~1")
      it "phrase" (test def ("foo" =: phrase ["bar", "baz"] ~: 1) "q=foo:\"bar baz\"~1")

    describe "range" $ do
      it "[* TO *]" (test def ("foo" =: star `to` star) "q=foo:[* TO *]")

      describe "int" $ do
        it "incl/incl" (test def ("foo" =: incl (int 5) `to` incl (int 6)) "q=foo:[5 TO 6]")
        it "excl/incl" (test def ("foo" =: excl (int 5) `to` incl (int 6)) "q=foo:{5 TO 6]")
        it "incl/excl" (test def ("foo" =: incl (int 5) `to` excl (int 6)) "q=foo:[5 TO 6}")
        it "excl/excl" (test def ("foo" =: excl (int 5) `to` excl (int 6)) "q=foo:{5 TO 6}")
        it "star/incl" (test def ("foo" =: star `to` incl (int 5)) "q=foo:[* TO 5]")
        it "incl/star" (test def ("foo" =: incl (int 5) `to` star) "q=foo:[5 TO *]")
        it "gt" (test def ("foo" =: gt (int 5)) "q=foo:{5 TO *]")
        it "gte" (test def ("foo" =: gte (int 5)) "q=foo:[5 TO *]")
        it "lt" (test def ("foo" =: lt (int 5)) "q=foo:[* TO 5}")
        it "lte" (test def ("foo" =: lte (int 5)) "q=foo:[* TO 5]")

      describe "float" $ do
        it "incl/incl" (test def ("foo" =: incl (float 5) `to` incl (float 6)) "q=foo:[5.0 TO 6.0]")
        it "excl/incl" (test def ("foo" =: excl (float 5) `to` incl (float 6)) "q=foo:{5.0 TO 6.0]")
        it "incl/excl" (test def ("foo" =: incl (float 5) `to` excl (float 6)) "q=foo:[5.0 TO 6.0}")
        it "excl/excl" (test def ("foo" =: excl (float 5) `to` excl (float 6)) "q=foo:{5.0 TO 6.0}")
        it "star/incl" (test def ("foo" =: star `to` incl (float 5)) "q=foo:[* TO 5.0]")
        it "incl/star" (test def ("foo" =: incl (float 5) `to` star) "q=foo:[5.0 TO *]")
        it "gt" (test def ("foo" =: gt (float 5)) "q=foo:{5.0 TO *]")
        it "gte" (test def ("foo" =: gte (float 5)) "q=foo:[5.0 TO *]")
        it "le" (test def ("foo" =: lt (float 5)) "q=foo:[* TO 5.0}")
        it "lte" (test def ("foo" =: lte (float 5)) "q=foo:[* TO 5.0]")

      describe "word" $ do
        it "incl/incl" (test def ("foo" =: incl (word "a") `to` incl (word "b")) "q=foo:[a TO b]")
        it "excl/incl" (test def ("foo" =: excl (word "a") `to` incl (word "b")) "q=foo:{a TO b]")
        it "incl/excl" (test def ("foo" =: incl (word "a") `to` excl (word "b")) "q=foo:[a TO b}")
        it "excl/excl" (test def ("foo" =: excl (word "a") `to` excl (word "b")) "q=foo:{a TO b}")
        it "star/incl" (test def ("foo" =: star `to` incl (word "a")) "q=foo:[* TO a]")
        it "incl/star" (test def ("foo" =: incl (word "a") `to` star) "q=foo:[a TO *]")
        it "gt" (test def ("foo" =: gt (word "a")) "q=foo:{a TO *]")
        it "gte" (test def ("foo" =: gte (word "a")) "q=foo:[a TO *]")
        it "lt" (test def ("foo" =: lt (word "a")) "q=foo:[* TO a}")
        it "lte" (test def ("foo" =: lte (word "a")) "q=foo:[* TO a]")

      describe "utctime" $ do
        it "incl/incl" (test def ("foo" =: incl (datetime t1) `to` incl (datetime t2)) "q=foo:[\"2015-01-01T00:00:00Z\" TO \"2016-01-01T00:00:00Z\"]")
        it "excl/incl" (test def ("foo" =: excl (datetime t1) `to` incl (datetime t2)) "q=foo:{\"2015-01-01T00:00:00Z\" TO \"2016-01-01T00:00:00Z\"]")
        it "incl/excl" (test def ("foo" =: incl (datetime t1) `to` excl (datetime t2)) "q=foo:[\"2015-01-01T00:00:00Z\" TO \"2016-01-01T00:00:00Z\"}")
        it "excl/excl" (test def ("foo" =: excl (datetime t1) `to` excl (datetime t2)) "q=foo:{\"2015-01-01T00:00:00Z\" TO \"2016-01-01T00:00:00Z\"}")
        it "star/incl" (test def ("foo" =: star `to` incl (datetime t1)) "q=foo:[* TO \"2015-01-01T00:00:00Z\"]")
        it "incl/star" (test def ("foo" =: incl (datetime t1) `to` star) "q=foo:[\"2015-01-01T00:00:00Z\" TO *]")
        it "star/star" (test def ("foo" =: star `to` star) "q=foo:[* TO *]")
        it "gt" (test def ("foo" =: gt (datetime t1)) "q=foo:{\"2015-01-01T00:00:00Z\" TO *]")
        it "gte" (test def ("foo" =: gte (datetime t1)) "q=foo:[\"2015-01-01T00:00:00Z\" TO *]")
        it "lt" (test def ("foo" =: lt (datetime t1)) "q=foo:[* TO \"2015-01-01T00:00:00Z\"}")
        it "lte" (test def ("foo" =: lte (datetime t1)) "q=foo:[* TO \"2015-01-01T00:00:00Z\"]")

    describe "boost" $ do
      it "word" (test def ("foo" =: word "bar" ^: 3) "q=foo:bar^3.0")
      it "phrase" (test def ("foo" =: phrase ["bar", "baz"] ^: 3) "q=foo:\"bar baz\"^3.0")

    it "&&:" (test def ("foo" =: word "bar" &&: "baz" =: word "qux") "q=(foo:bar AND baz:qux)")
    it "||:" (test def ("foo" =: word "bar" ||: "baz" =: word "qux") "q=(foo:bar OR baz:qux)")
    it "-:" (test def ("foo" =: word "bar" -: "baz" =: word "qux") "q=(foo:bar NOT baz:qux)")

    describe "constant score" $ do
      it "word" (test def ("foo" =: word "bar" ^=: 3.5) "q=foo:bar^=3.5")
      it "phrase" (test def ("foo" =: phrase ["bar", "baz"] ^=: 3.5) "q=foo:\"bar baz\"^=3.5")

    describe "params" $ do
      it "one" (test (def & df "foo") (defaultField (word "bar")) "q={!df=foo}bar")
      it "two" (test (def & df "foo" & opAnd) (defaultField (word "bar")) "q={!df=foo q.op=AND}bar")
 where
  t1 = UTCTime (fromGregorian 2015 1 1) 0
  t2 = UTCTime (fromGregorian 2016 1 1) 0

test :: LocalParams LuceneQuery -> LuceneQuery -> Text -> Expectation
test locals query result = compile [] locals query `shouldBe` result

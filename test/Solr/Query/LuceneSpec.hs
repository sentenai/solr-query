module Solr.Query.LuceneSpec where

import Solr.Query.Lucene

import Data.Text.Lazy (Text)
import Data.Time
import Prelude.Compat
import Test.Hspec

spec :: Spec
spec =
  describe "compile" $ do
    it "defaultField" (test [] (defaultField (word "foo")) "q={!type=lucene}foo")
    it "field" (test [] ("foo" =: word "bar") "q={!type=lucene}foo:bar")
    it "int" (test [] ("foo" =: int 5) "q={!type=lucene}foo:5")
    it "float" (test [] ("foo" =: float 5) "q={!type=lucene}foo:5.0")
    it "true" (test [] ("foo" =: true) "q={!type=lucene}foo:true")
    it "false" (test [] ("foo" =: false) "q={!type=lucene}foo:false")
    it "wild" (test [] ("foo" =: wild "b?r") "q={!type=lucene}foo:b?r")
    it "regex" (test [] ("foo" =: regex "[mb]oat") "q={!type=lucene}foo:/[mb]oat/")
    it "phrase" (test [] ("foo" =: phrase ["bar", "baz"]) "q={!type=lucene}foo:\"bar baz\"")

    describe "datetime" $ do
      it "utc" (test [] ("foo" =: datetime t1) "q={!type=lucene}foo:\"2015-01-01T00:00:00Z\"")
      it "Y" (test [] ("foo" =: datetime (2015::Year)) "q={!type=lucene}foo:\"2015\"")
      it "YM" (test [] ("foo" =: datetime (2015, 1)) "q={!type=lucene}foo:\"2015-01\"")
      it "YMD" (test [] ("foo" =: datetime (2015, 1, 2)) "q={!type=lucene}foo:\"2015-01-02\"")
      it "YMD:H" (test [] ("foo" =: datetime (2015, 1, 2, 3)) "q={!type=lucene}foo:\"2015-01-02T03\"")
      it "YMD:HM" (test [] ("foo" =: datetime (2015, 1, 2, 3, 4)) "q={!type=lucene}foo:\"2015-01-02T03:04\"")
      it "YMD:HMS" (test [] ("foo" =: datetime (2015, 1, 2, 3, 4, 5)) "q={!type=lucene}foo:\"2015-01-02T03:04:05\"")
      it "YMD:HMSs" (test [] ("foo" =: datetime (2015, 1, 2, 3, 4, 5, 6)) "q={!type=lucene}foo:\"2015-01-02T03:04:05.06000Z\"")
      it "clamps month -> 1" (test [] ("foo" =: datetime (2015, 0)) "q={!type=lucene}foo:\"2015-01\"")
      it "clamps month 12 <-" (test [] ("foo" =: datetime (2015, 13)) "q={!type=lucene}foo:\"2015-12\"")
      it "clamps day -> 1" (test [] ("foo" =: datetime (2015, 1, 0)) "q={!type=lucene}foo:\"2015-01-01\"")
      it "clamps day 31 <-" (test [] ("foo" =: datetime (2015, 1, 32)) "q={!type=lucene}foo:\"2015-01-31\"")
      it "clamps hour -> 0" (test [] ("foo" =: datetime (2015, 1, 1, -1)) "q={!type=lucene}foo:\"2015-01-01T00\"")
      it "clamps hour 23 <-" (test [] ("foo" =: datetime (2015, 1, 1, 24)) "q={!type=lucene}foo:\"2015-01-01T23\"")
      it "clamps minute -> 0" (test [] ("foo" =: datetime (2015, 1, 1, 1, -1))        "q={!type=lucene}foo:\"2015-01-01T01:00\"")
      it "clamps minute 59 <-" (test [] ("foo" =: datetime (2015, 1, 1, 1, 60)) "q={!type=lucene}foo:\"2015-01-01T01:59\"")
      it "clamps second -> 0" (test [] ("foo" =: datetime (2015, 1, 1, 1, 1, -1)) "q={!type=lucene}foo:\"2015-01-01T01:01:00\"")
      it "clamps second 60 <-" (test [] ("foo" =: datetime (2015, 1, 1, 1, 1, 61)) "q={!type=lucene}foo:\"2015-01-01T01:01:60\"")
      it "clamps millisecond -> 0" (test [] ("foo" =: datetime (2015, 1, 1, 1, 1, 1, -1)) "q={!type=lucene}foo:\"2015-01-01T01:01:01.00000Z\"")
      it "clamps millisecond 99.999 <-" (test [] ("foo" =: datetime (2015, 1, 1, 1, 1, 1, 100)) "q={!type=lucene}foo:\"2015-01-01T01:01:01.99999Z\"")

    describe "fuzzy" $ do
      it "word" (test [] ("foo" =: word "bar" ~: 1) "q={!type=lucene}foo:bar~1")
      it "phrase" (test [] ("foo" =: phrase ["bar", "baz"] ~: 1) "q={!type=lucene}foo:\"bar baz\"~1")

    describe "range" $ do
      it "**" (test [] ("foo" =: star `to` star) "q={!type=lucene}foo:[* TO *]")

      describe "int" $ do
        it "[]" (test [] ("foo" =: incl (int 5) `to` incl (int 6)) "q={!type=lucene}foo:[5 TO 6]")
        it "{]" (test [] ("foo" =: excl (int 5) `to` incl (int 6)) "q={!type=lucene}foo:{5 TO 6]")
        it "[}" (test [] ("foo" =: incl (int 5) `to` excl (int 6)) "q={!type=lucene}foo:[5 TO 6}")
        it "{}" (test [] ("foo" =: excl (int 5) `to` excl (int 6)) "q={!type=lucene}foo:{5 TO 6}")
        it "*]" (test [] ("foo" =: star `to` incl (int 5)) "q={!type=lucene}foo:[* TO 5]")
        it "[*" (test [] ("foo" =: incl (int 5) `to` star) "q={!type=lucene}foo:[5 TO *]")
        it ">"  (test [] ("foo" =: gt (int 5)) "q={!type=lucene}foo:{5 TO *]")
        it ">=" (test [] ("foo" =: gte (int 5)) "q={!type=lucene}foo:[5 TO *]")
        it "<"  (test [] ("foo" =: lt (int 5)) "q={!type=lucene}foo:[* TO 5}")
        it "<"  (test [] ("foo" =: lte (int 5)) "q={!type=lucene}foo:[* TO 5]")

      describe "float" $ do
        it "[]" (test [] ("foo" =: incl (float 5) `to` incl (float 6)) "q={!type=lucene}foo:[5.0 TO 6.0]")
        it "{]" (test [] ("foo" =: excl (float 5) `to` incl (float 6)) "q={!type=lucene}foo:{5.0 TO 6.0]")
        it "[}" (test [] ("foo" =: incl (float 5) `to` excl (float 6)) "q={!type=lucene}foo:[5.0 TO 6.0}")
        it "{}" (test [] ("foo" =: excl (float 5) `to` excl (float 6)) "q={!type=lucene}foo:{5.0 TO 6.0}")
        it "*]" (test [] ("foo" =: star `to` incl (float 5)) "q={!type=lucene}foo:[* TO 5.0]")
        it "[*" (test [] ("foo" =: incl (float 5) `to` star) "q={!type=lucene}foo:[5.0 TO *]")
        it ">"  (test [] ("foo" =: gt (float 5)) "q={!type=lucene}foo:{5.0 TO *]")
        it ">=" (test [] ("foo" =: gte (float 5)) "q={!type=lucene}foo:[5.0 TO *]")
        it "<"  (test [] ("foo" =: lt (float 5)) "q={!type=lucene}foo:[* TO 5.0}")
        it "<"  (test [] ("foo" =: lte (float 5)) "q={!type=lucene}foo:[* TO 5.0]")

      describe "word" $ do
        it "[]" (test [] ("foo" =: incl (word "a") `to` incl (word "b")) "q={!type=lucene}foo:[a TO b]")
        it "{]" (test [] ("foo" =: excl (word "a") `to` incl (word "b")) "q={!type=lucene}foo:{a TO b]")
        it "[}" (test [] ("foo" =: incl (word "a") `to` excl (word "b")) "q={!type=lucene}foo:[a TO b}")
        it "{}" (test [] ("foo" =: excl (word "a") `to` excl (word "b")) "q={!type=lucene}foo:{a TO b}")
        it "*]" (test [] ("foo" =: star `to` incl (word "a")) "q={!type=lucene}foo:[* TO a]")
        it "[*" (test [] ("foo" =: incl (word "a") `to` star) "q={!type=lucene}foo:[a TO *]")
        it ">"  (test [] ("foo" =: gt (word "a")) "q={!type=lucene}foo:{a TO *]")
        it ">=" (test [] ("foo" =: gte (word "a")) "q={!type=lucene}foo:[a TO *]")
        it "<"  (test [] ("foo" =: lt (word "a")) "q={!type=lucene}foo:[* TO a}")
        it "<=" (test [] ("foo" =: lte (word "a")) "q={!type=lucene}foo:[* TO a]")

      describe "utctime" $ do
        it "[]" (test [] ("foo" =: incl (datetime t1) `to` incl (datetime t2)) "q={!type=lucene}foo:[\"2015-01-01T00:00:00Z\" TO \"2016-01-01T00:00:00Z\"]")
        it "{]" (test [] ("foo" =: excl (datetime t1) `to` incl (datetime t2)) "q={!type=lucene}foo:{\"2015-01-01T00:00:00Z\" TO \"2016-01-01T00:00:00Z\"]")
        it "[}" (test [] ("foo" =: incl (datetime t1) `to` excl (datetime t2)) "q={!type=lucene}foo:[\"2015-01-01T00:00:00Z\" TO \"2016-01-01T00:00:00Z\"}")
        it "{}" (test [] ("foo" =: excl (datetime t1) `to` excl (datetime t2)) "q={!type=lucene}foo:{\"2015-01-01T00:00:00Z\" TO \"2016-01-01T00:00:00Z\"}")
        it "*]" (test [] ("foo" =: star `to` incl (datetime t1)) "q={!type=lucene}foo:[* TO \"2015-01-01T00:00:00Z\"]")
        it "[*" (test [] ("foo" =: incl (datetime t1) `to` star) "q={!type=lucene}foo:[\"2015-01-01T00:00:00Z\" TO *]")
        it "**" (test [] ("foo" =: star `to` star) "q={!type=lucene}foo:[* TO *]")
        it ">"  (test [] ("foo" =: gt (datetime t1)) "q={!type=lucene}foo:{\"2015-01-01T00:00:00Z\" TO *]")
        it ">=" (test [] ("foo" =: gte (datetime t1)) "q={!type=lucene}foo:[\"2015-01-01T00:00:00Z\" TO *]")
        it "<"  (test [] ("foo" =: lt (datetime t1)) "q={!type=lucene}foo:[* TO \"2015-01-01T00:00:00Z\"}")
        it "<=" (test [] ("foo" =: lte (datetime t1)) "q={!type=lucene}foo:[* TO \"2015-01-01T00:00:00Z\"]")

    describe "boost" $ do
      it "word" (test [] ("foo" =: word "bar" ^: 3) "q={!type=lucene}foo:bar^3.0")
      it "phrase" (test [] ("foo" =: phrase ["bar", "baz"] ^: 3) "q={!type=lucene}foo:\"bar baz\"^3.0")

    it "AND" (test [] ("foo" =: word "bar" &&: "baz" =: word "qux") "q={!type=lucene}(foo:bar AND baz:qux)")
    it "OR"  (test [] ("foo" =: word "bar" ||: "baz" =: word "qux") "q={!type=lucene}(foo:bar OR baz:qux)")
    it "NOT" (test [] ("foo" =: word "bar" -: "baz" =: word "qux") "q={!type=lucene}(foo:bar NOT baz:qux)")

    describe "constant score" $ do
      it "word" (test [] ("foo" =: word "bar" ^=: 3.5) "q={!type=lucene}foo:bar^=3.5")
      it "phrase" (test [] ("foo" =: phrase ["bar", "baz"] ^=: 3.5) "q={!type=lucene}foo:\"bar baz\"^=3.5")

    it "neg" (test [] (neg ("foo" =: word "bar")) "q={!type=lucene}(*:* NOT foo:bar)")

    describe "params" $ do
      it "one" (test [df "foo"] (defaultField (word "bar")) "q={!type=lucene df=foo}bar")
      it "two" (test [df "foo", opAnd] (defaultField (word "bar")) "q={!type=lucene df=foo q.op=AND}bar")
 where
  t1 = UTCTime (fromGregorian 2015 1 1) 0
  t2 = UTCTime (fromGregorian 2016 1 1) 0

test :: [LuceneQueryParam] -> LuceneQuery -> Text -> Expectation
test locals query result = compile [] locals query `shouldBe` result

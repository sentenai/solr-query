module Solr.QuerySpec where

import Solr.DateTime
import Solr.Query
import Solr.Type

import Data.Text.Lazy (Text)
import Data.Time
import Test.Hspec

spec :: Spec
spec =
  describe "compile" $ do
    it "defaultField" (test []  (defaultField (word "foo"))      "q=foo")
    it "field"        (test []  ("foo" =: word "bar")            "q=foo:bar")
    it "int"          (test []  ("foo" =: int 5)                 "q=foo:5")
    it "float"        (test []  ("foo" =: float 5)               "q=foo:5.0")
    it "true"         (test []  ("foo" =: true)                  "q=foo:true")
    it "false"        (test []  ("foo" =: false)                 "q=foo:false")
    it "wild"         (test []  ("foo" =: wild "b?r")            "q=foo:b?r")
    it "regex"        (test []  ("foo" =: regex "[mb]oat")       "q=foo:/[mb]oat/")
    it "phrase"       (test []  ("foo" =: phrase ["bar", "baz"]) "q=foo:\"bar baz\"")

    describe "datetime" $ do
      it "utc"                          (test [] ("foo" =: datetime t1)                         "q=foo:\"2015-01-01T00:00:00Z\"")
      it "Y"                            (test [] ("foo" =: datetime (2015::Year))               "q=foo:\"2015\"")
      it "YM"                           (test [] ("foo" =: datetime (2015, 1))                  "q=foo:\"2015-01\"")
      it "YMD"                          (test [] ("foo" =: datetime (2015, 1, 2))               "q=foo:\"2015-01-02\"")
      it "YMD:H"                        (test [] ("foo" =: datetime (2015, 1, 2, 3))            "q=foo:\"2015-01-02T03\"")
      it "YMD:HM"                       (test [] ("foo" =: datetime (2015, 1, 2, 3, 4))         "q=foo:\"2015-01-02T03:04\"")
      it "YMD:HMS"                      (test [] ("foo" =: datetime (2015, 1, 2, 3, 4, 5))      "q=foo:\"2015-01-02T03:04:05\"")
      it "YMD:HMSs"                     (test [] ("foo" =: datetime (2015, 1, 2, 3, 4, 5, 6))   "q=foo:\"2015-01-02T03:04:05.06000Z\"")
      it "clamps month -> 1"            (test [] ("foo" =: datetime (2015, 0))                  "q=foo:\"2015-01\"")
      it "clamps month 12 <-"           (test [] ("foo" =: datetime (2015, 13))                 "q=foo:\"2015-12\"")
      it "clamps day -> 1"              (test [] ("foo" =: datetime (2015, 1, 0))               "q=foo:\"2015-01-01\"")
      it "clamps day 31 <-"             (test [] ("foo" =: datetime (2015, 1, 32))              "q=foo:\"2015-01-31\"")
      it "clamps hour -> 0"             (test [] ("foo" =: datetime (2015, 1, 1, -1))           "q=foo:\"2015-01-01T00\"")
      it "clamps hour 23 <-"            (test [] ("foo" =: datetime (2015, 1, 1, 24))           "q=foo:\"2015-01-01T23\"")
      it "clamps minute -> 0"           (test [] ("foo" =: datetime (2015, 1, 1, 1, -1))        "q=foo:\"2015-01-01T01:00\"")
      it "clamps minute 59 <-"          (test [] ("foo" =: datetime (2015, 1, 1, 1, 60))        "q=foo:\"2015-01-01T01:59\"")
      it "clamps second -> 0"           (test [] ("foo" =: datetime (2015, 1, 1, 1, 1, -1))     "q=foo:\"2015-01-01T01:01:00\"")
      it "clamps second 60 <-"          (test [] ("foo" =: datetime (2015, 1, 1, 1, 1, 61))     "q=foo:\"2015-01-01T01:01:60\"")
      it "clamps millisecond -> 0"      (test [] ("foo" =: datetime (2015, 1, 1, 1, 1, 1, -1))  "q=foo:\"2015-01-01T01:01:01.00000Z\"")
      it "clamps millisecond 99.999 <-" (test [] ("foo" =: datetime (2015, 1, 1, 1, 1, 1, 100)) "q=foo:\"2015-01-01T01:01:01.99999Z\"")

    describe "fuzzy" $ do
      it "word"   (test []  ("foo" =: word "bar" ~: 1)            "q=foo:bar~1")
      it "phrase" (test []  ("foo" =: phrase ["bar", "baz"] ~: 1) "q=foo:\"bar baz\"~1")

    describe "range" $ do
      describe "int" $ do
        it "[]" (test []  ("foo" =: incl (int 5) `to` incl (int 6)) "q=foo:[5 TO 6]")
        it "{]" (test []  ("foo" =: excl (int 5) `to` incl (int 6)) "q=foo:{5 TO 6]")
        it "[}" (test []  ("foo" =: incl (int 5) `to` excl (int 6)) "q=foo:[5 TO 6}")
        it "{}" (test []  ("foo" =: excl (int 5) `to` excl (int 6)) "q=foo:{5 TO 6}")
        it "*]" (test []  ("foo" =: star `to` incl (int 5))         "q=foo:[* TO 5]")
        it "[*" (test []  ("foo" =: incl (int 5) `to` star)         "q=foo:[5 TO *]")
        it "**" (test []  ("foo" =: star `to` numStar)              "q=foo:[* TO *]")
        it ">"  (test []  ("foo" =: gt (int 5))                     "q=foo:{5 TO *]")
        it ">=" (test []  ("foo" =: gte (int 5))                    "q=foo:[5 TO *]")
        it "<"  (test []  ("foo" =: lt (int 5))                     "q=foo:[* TO 5}")
        it "<"  (test []  ("foo" =: lte (int 5))                    "q=foo:[* TO 5]")

      describe "float" $ do
        it "[]" (test []  ("foo" =: incl (float 5) `to` incl (float 6)) "q=foo:[5.0 TO 6.0]")
        it "{]" (test []  ("foo" =: excl (float 5) `to` incl (float 6)) "q=foo:{5.0 TO 6.0]")
        it "[}" (test []  ("foo" =: incl (float 5) `to` excl (float 6)) "q=foo:[5.0 TO 6.0}")
        it "{}" (test []  ("foo" =: excl (float 5) `to` excl (float 6)) "q=foo:{5.0 TO 6.0}")
        it "*]" (test []  ("foo" =: star `to` incl (float 5))           "q=foo:[* TO 5.0]")
        it "[*" (test []  ("foo" =: incl (float 5) `to` star)           "q=foo:[5.0 TO *]")
        it "**" (test []  ("foo" =: star `to` numStar)                  "q=foo:[* TO *]")
        it ">"  (test []  ("foo" =: gt (float 5))                       "q=foo:{5.0 TO *]")
        it ">=" (test []  ("foo" =: gte (float 5))                      "q=foo:[5.0 TO *]")
        it "<"  (test []  ("foo" =: lt (float 5))                       "q=foo:[* TO 5.0}")
        it "<"  (test []  ("foo" =: lte (float 5))                      "q=foo:[* TO 5.0]")

      describe "word" $ do
        it "[]" (test []  ("foo" =: incl (word "a") `to` incl (word "b")) "q=foo:[a TO b]")
        it "{]" (test []  ("foo" =: excl (word "a") `to` incl (word "b")) "q=foo:{a TO b]")
        it "[}" (test []  ("foo" =: incl (word "a") `to` excl (word "b")) "q=foo:[a TO b}")
        it "{}" (test []  ("foo" =: excl (word "a") `to` excl (word "b")) "q=foo:{a TO b}")
        it "*]" (test []  ("foo" =: star `to` incl (word "a"))            "q=foo:[* TO a]")
        it "[*" (test []  ("foo" =: incl (word "a") `to` star)            "q=foo:[a TO *]")
        it "**" (test []  ("foo" =: star `to` wordStar)                   "q=foo:[* TO *]")
        it ">"  (test []  ("foo" =: gt (word "a"))                        "q=foo:{a TO *]")
        it ">=" (test []  ("foo" =: gte (word "a"))                       "q=foo:[a TO *]")
        it "<"  (test []  ("foo" =: lt (word "a"))                        "q=foo:[* TO a}")
        it "<=" (test []  ("foo" =: lte (word "a"))                       "q=foo:[* TO a]")

      describe "utctime" $ do
        it "[]" (test []  ("foo" =: incl (datetime t1) `to` incl (datetime t2)) "q=foo:[\"2015-01-01T00:00:00Z\" TO \"2016-01-01T00:00:00Z\"]")
        it "{]" (test []  ("foo" =: excl (datetime t1) `to` incl (datetime t2)) "q=foo:{\"2015-01-01T00:00:00Z\" TO \"2016-01-01T00:00:00Z\"]")
        it "[}" (test []  ("foo" =: incl (datetime t1) `to` excl (datetime t2)) "q=foo:[\"2015-01-01T00:00:00Z\" TO \"2016-01-01T00:00:00Z\"}")
        it "{}" (test []  ("foo" =: excl (datetime t1) `to` excl (datetime t2)) "q=foo:{\"2015-01-01T00:00:00Z\" TO \"2016-01-01T00:00:00Z\"}")
        it "*]" (test []  ("foo" =: star `to` incl (datetime t1))               "q=foo:[* TO \"2015-01-01T00:00:00Z\"]")
        it "[*" (test []  ("foo" =: incl (datetime t1) `to` star)               "q=foo:[\"2015-01-01T00:00:00Z\" TO *]")
        it "**" (test []  ("foo" =: star `to` dateStar)                         "q=foo:[* TO *]")
        it ">"  (test []  ("foo" =: gt (datetime t1))                           "q=foo:{\"2015-01-01T00:00:00Z\" TO *]")
        it ">=" (test []  ("foo" =: gte (datetime t1))                          "q=foo:[\"2015-01-01T00:00:00Z\" TO *]")
        it "<"  (test []  ("foo" =: lt (datetime t1))                           "q=foo:[* TO \"2015-01-01T00:00:00Z\"}")
        it "<=" (test []  ("foo" =: lte (datetime t1))                          "q=foo:[* TO \"2015-01-01T00:00:00Z\"]")

    describe "boost" $ do
      it "word"   (test []  ("foo" =: word "bar" ^: 3)            "q=foo:bar^3.0")
      it "phrase" (test []  ("foo" =: phrase ["bar", "baz"] ^: 3) "q=foo:\"bar baz\"^3.0")

    it "AND" (test []  ("foo" =: word "bar" &&: "baz" =: word "qux") "q=(foo:bar AND baz:qux)")
    it "OR"  (test []  ("foo" =: word "bar" ||: "baz" =: word "qux") "q=(foo:bar OR baz:qux)")
    it "NOT" (test []  ("foo" =: word "bar" -: "baz" =: word "qux")  "q=(foo:bar NOT baz:qux)")

    describe "constant score" $ do
      it "word"   (test []  ("foo" =: word "bar" ^=: 3.5)            "q=foo:bar^=3.5")
      it "phrase" (test []  ("foo" =: phrase ["bar", "baz"] ^=: 3.5) "q=foo:\"bar baz\"^=3.5")

    it "neg" (test []  (neg ("foo" =: word "bar")) "q=-foo:bar")

    describe "params" $ do
      it "one" (test ps1 (defaultField (word "bar")) "q={!df=foo}bar")
      it "two" (test ps2 (defaultField (word "bar")) "q={!df=foo q.op=AND}bar")
 where
  ps1 = [paramDefaultField "foo"]
  ps2 = [paramDefaultField "foo", paramOpAnd]

  t1 = UTCTime (fromGregorian 2015 1 1) 0
  t2 = UTCTime (fromGregorian 2016 1 1) 0

  numStar :: Boundary (Expr 'TNum)
  numStar = star

  wordStar :: Boundary (Expr 'TWord)
  wordStar = star

  dateStar :: Boundary (Expr 'TDateTime)
  dateStar = star

test :: [Param Query] -> Query Expr -> Text -> Expectation
test params query result = compile params query `shouldBe` result

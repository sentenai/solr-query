module Solr.QuerySpec where

import Solr.Query
import Solr.Type

import Data.Text.Lazy (Text)
import Data.Time
import Test.Hspec

spec :: Spec
spec =
  describe "compile" $ do
    it "defaultField"   (test []  (defaultField (word "foo"))                         "q=foo")
    it "field"          (test []  ("foo" =: word "bar")                               "q=foo:bar")
    it "num"            (test []  ("foo" =: num 5)                                    "q=foo:5.0")
    it "true"           (test []  ("foo" =: true)                                     "q=foo:true")
    it "false"          (test []  ("foo" =: false)                                    "q=foo:false")
    it "wild"           (test []  ("foo" =: wild "b?r")                               "q=foo:b?r")
    it "regex"          (test []  ("foo" =: regex "[mb]oat")                          "q=foo:/[mb]oat/")
    it "phrase"         (test []  ("foo" =: phrase ["bar", "baz"])                    "q=foo:\"bar baz\"")
    it "fuzzy word"     (test []  ("foo" =: word "bar" ~: 1)                          "q=foo:bar~1")
    it "fuzzy phrase"   (test []  ("foo" =: phrase ["bar", "baz"] ~: 1)               "q=foo:\"bar baz\"~1")
    it "num incl incl"  (test []  ("foo" =: incl (num 5) `to` incl (num 6))           "q=foo:[5.0 TO 6.0]")
    it "num excl incl"  (test []  ("foo" =: excl (num 5) `to` incl (num 6))           "q=foo:{5.0 TO 6.0]")
    it "num incl excl"  (test []  ("foo" =: incl (num 5) `to` excl (num 6))           "q=foo:[5.0 TO 6.0}")
    it "num excl excl"  (test []  ("foo" =: excl (num 5) `to` excl (num 6))           "q=foo:{5.0 TO 6.0}")
    it "num star incl"  (test []  ("foo" =: star `to` incl (num 5))                   "q=foo:[* TO 5.0]")
    it "num incl star"  (test []  ("foo" =: incl (num 5) `to` star)                   "q=foo:[5.0 TO *]")
    it "num star star"  (test []  ("foo" =: star `to` numStar)                        "q=foo:[* TO *]")
    it "num gt"         (test []  ("foo" =: gt (num 5))                               "q=foo:{5.0 TO *]")
    it "num gte"        (test []  ("foo" =: gte (num 5))                              "q=foo:[5.0 TO *]")
    it "num lt"         (test []  ("foo" =: lt (num 5))                               "q=foo:[* TO 5.0}")
    it "num lte"        (test []  ("foo" =: lte (num 5))                              "q=foo:[* TO 5.0]")
    it "word incl incl" (test []  ("foo" =: incl (word "a") `to` incl (word "b"))     "q=foo:[a TO b]")
    it "word excl incl" (test []  ("foo" =: excl (word "a") `to` incl (word "b"))     "q=foo:{a TO b]")
    it "word incl excl" (test []  ("foo" =: incl (word "a") `to` excl (word "b"))     "q=foo:[a TO b}")
    it "word excl excl" (test []  ("foo" =: excl (word "a") `to` excl (word "b"))     "q=foo:{a TO b}")
    it "word star incl" (test []  ("foo" =: star `to` incl (word "a"))                "q=foo:[* TO a]")
    it "word incl star" (test []  ("foo" =: incl (word "a") `to` star)                "q=foo:[a TO *]")
    it "word star star" (test []  ("foo" =: star `to` wordStar)                       "q=foo:[* TO *]")
    it "word gt"        (test []  ("foo" =: gt (word "a"))                            "q=foo:{a TO *]")
    it "word gte"       (test []  ("foo" =: gte (word "a"))                           "q=foo:[a TO *]")
    it "word lt"        (test []  ("foo" =: lt (word "a"))                            "q=foo:[* TO a}")
    it "word lte"       (test []  ("foo" =: lte (word "a"))                           "q=foo:[* TO a]")
    it "date incl incl" (test []  ("foo" =: incl (utctime t1) `to` incl (utctime t2)) "q=foo:[2015-01-01T00:00:00Z TO 2016-01-01T00:00:00Z]")
    it "date excl incl" (test []  ("foo" =: excl (utctime t1) `to` incl (utctime t2)) "q=foo:{2015-01-01T00:00:00Z TO 2016-01-01T00:00:00Z]")
    it "date incl excl" (test []  ("foo" =: incl (utctime t1) `to` excl (utctime t2)) "q=foo:[2015-01-01T00:00:00Z TO 2016-01-01T00:00:00Z}")
    it "date excl excl" (test []  ("foo" =: excl (utctime t1) `to` excl (utctime t2)) "q=foo:{2015-01-01T00:00:00Z TO 2016-01-01T00:00:00Z}")
    it "date star incl" (test []  ("foo" =: star `to` incl (utctime t1))              "q=foo:[* TO 2015-01-01T00:00:00Z]")
    it "date incl star" (test []  ("foo" =: incl (utctime t1) `to` star)              "q=foo:[2015-01-01T00:00:00Z TO *]")
    it "date star star" (test []  ("foo" =: star `to` numStar)                        "q=foo:[* TO *]")
    it "date gt"        (test []  ("foo" =: gt (utctime t1))                          "q=foo:{2015-01-01T00:00:00Z TO *]")
    it "date gte"       (test []  ("foo" =: gte (utctime t1))                         "q=foo:[2015-01-01T00:00:00Z TO *]")
    it "date lt"        (test []  ("foo" =: lt (utctime t1))                          "q=foo:[* TO 2015-01-01T00:00:00Z}")
    it "date lte"       (test []  ("foo" =: lte (utctime t1))                         "q=foo:[* TO 2015-01-01T00:00:00Z]")
    it "fuzzy word"     (test []  ("foo" =: word "bar" ^: 3)                          "q=foo:bar^3.0")
    it "fuzzy phrase"   (test []  ("foo" =: phrase ["bar", "baz"] ^: 3)               "q=foo:\"bar baz\"^3.0")
    it "AND"            (test []  ("foo" =: word "bar" &&: "baz" =: word "qux")       "q=(foo:bar AND baz:qux)")
    it "OR"             (test []  ("foo" =: word "bar" ||: "baz" =: word "qux")       "q=(foo:bar OR baz:qux)")
    it "NOT"            (test []  ("foo" =: word "bar" -: "baz" =: word "qux")        "q=(foo:bar NOT baz:qux)")
    it "boosted word"   (test []  ("foo" =: word "bar" ^=: 3.5)                       "q=foo:bar^=3.5")
    it "boosted phrase" (test []  ("foo" =: phrase ["bar", "baz"] ^=: 3.5)            "q=foo:\"bar baz\"^=3.5")
    it "neg"            (test []  (neg ("foo" =: word "bar"))                         "q=-foo:bar")
    it "one param"      (test ps1 (defaultField (word "bar"))                         "q={!df=foo}bar")
    it "two params"     (test ps2 (defaultField (word "bar"))                         "q={!df=foo q.op=AND}bar")
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

module Solr.Query.Internal
  ( Query
  , LocalParams
  , defaultMempty
  , defaultMappend
  , compileLocalParams
  , should
  , must
  , mustNot
  , filt
  , compileQuery
  , SomeQuery
  , someQuery
  ) where

import Solr.Prelude

import Builder

class (Coercible query Builder, Default (LocalParams query))
    => Query query where
  data LocalParams query :: *
  compileLocalParams :: LocalParams query -> [Builder]

defaultMempty :: Coercible query Builder => query
defaultMempty = coerce (mempty :: Builder)

defaultMappend :: Coercible query Builder => query -> query -> query
defaultMappend q1 q2 = coerce (coerce q1 `mappend` char ' ' `mappend` coerce q2)

-- | This clause <https://lucene.apache.org/core/6_4_2/core/org/apache/lucene/search/BooleanClause.Occur.html#SHOULD should>
-- occur.
should :: Query query => query -> query
should = id

-- | This clause <https://lucene.apache.org/core/6_4_2/core/org/apache/lucene/search/BooleanClause.Occur.html#MUST must>
-- occur.
must :: Query query => query -> query
must = coerce . (char '+' <>) . parens . coerce

-- | This clause <https://lucene.apache.org/core/6_4_2/core/org/apache/lucene/search/BooleanClause.Occur.html#MUST_NOT must not>
-- occur.
mustNot :: Query query => query -> query
mustNot = coerce . (char '-' <>) . parens . coerce

-- | This clause <https://lucene.apache.org/core/6_4_2/core/org/apache/lucene/search/BooleanClause.Occur.html#FILTER must>
-- occur, but it does not participate in scoring.
filt :: Query query => query -> query
filt = coerce . (char '#' <>) . parens . coerce


-- | 'SomeQuery' is a simple wrapper around a 'Query' that enables composition
-- through its 'Monoid' instance.
--
-- It has no 'LocalParams' of its own - you can only create them with 'def'.
newtype SomeQuery
  = Q { unQ :: Builder }

instance Monoid SomeQuery where
  mempty :: SomeQuery
  mempty = defaultMempty

  mappend :: SomeQuery -> SomeQuery -> SomeQuery
  mappend = defaultMappend

instance Query SomeQuery where
  data LocalParams SomeQuery

  compileLocalParams :: LocalParams SomeQuery -> [Builder]
  compileLocalParams _ = []

instance Default (LocalParams SomeQuery) where
  def = undefined

-- | Create a 'SomeQuery' from a 'Query' and its 'LocalParams'.
someQuery :: Query query => LocalParams query -> query -> SomeQuery
someQuery locals query = Q (compileQuery locals query)

compileQuery :: Query query => LocalParams query -> query -> Builder
compileQuery locals query =
  case compileLocalParams locals of
    [] -> coerce query
    xs -> "{!" <> intersperse ' ' xs <> char '}' <> coerce query

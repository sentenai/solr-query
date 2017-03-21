module Solr.Query.Internal.Internal where

import Solr.Prelude

import Builder

class (Coercible query Builder, Default (LocalParams query))
    => Query query where
  data LocalParams query :: *

  compileLocalParams :: LocalParams query -> [(Builder, Builder)]

coerceQuery :: Query query => query -> Builder
coerceQuery = coerce

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
  = SomeQ Builder

instance Monoid SomeQuery where
  mempty :: SomeQuery
  mempty = defaultMempty

  mappend :: SomeQuery -> SomeQuery -> SomeQuery
  mappend = defaultMappend

instance Query SomeQuery where
  data LocalParams SomeQuery

  compileLocalParams :: LocalParams SomeQuery -> [(Builder, Builder)]
  compileLocalParams _ = []

instance Default (LocalParams SomeQuery) where
  def = error "LocalParams SomeQuery: def"

-- | Create a 'SomeQuery' from a 'Query' and its 'LocalParams'.
someQuery :: Query query => LocalParams query -> query -> SomeQuery
someQuery locals query = SomeQ (compileQuery locals query)

-- | Compile a 'Query' and its 'LocalParams' to a 'Builder'.
compileQuery :: Query query => LocalParams query -> query -> Builder
compileQuery locals query =
  case compileLocalParams locals of
    [] -> coerce query
    xs -> mconcat
      [ "{!"
      , intersperse ' ' (map (\(x, y) -> x <> char '=' <> y) xs)
      , char '}'
      , coerce query
      ]

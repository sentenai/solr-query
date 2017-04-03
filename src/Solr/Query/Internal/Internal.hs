module Solr.Query.Internal.Internal where

import Solr.Prelude

import Builder

class (Coercible query Builder, Default (LocalParams query))
    => Query query where
  data LocalParams query :: *

  compileLocalParams :: LocalParams query -> [(Builder, Builder)]

coerceQuery :: Query query => query -> Builder
coerceQuery = coerce

(&&:) :: Query query => query -> query -> query
(&&:) q1 q2 = coerce (parens (coerce q1 <> " AND " <> coerce q2))
infixr 3 &&:

(||:) :: Query query => query -> query -> query
(||:) q1 q2 = coerce (parens (coerce q1 <> " OR " <> coerce q2))
infixr 2 ||:

(-:) :: Query query => query -> query -> query
(-:) q1 q2 = coerce (parens (coerce q1 <> " NOT " <> coerce q2))
infixl 6 -:

-- | 'SomeQuery' is a simple wrapper around a 'Query' that enables composition
-- through its 'Monoid' instance.
--
-- It has no 'LocalParams' of its own - you can only create them with 'def'.
newtype SomeQuery
  = SomeQ Builder

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

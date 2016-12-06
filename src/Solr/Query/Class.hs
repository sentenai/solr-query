-- | This module defines the finally tagless Solr DSL. This style admits
-- multiple interpreters, two of which (lazy 'Data.Text.Lazy.Text's and an
-- initial encoding) are provided by this library, in the "Solr.Query" and
-- "Solr.Query.Initial" modules, respectively.
--
-- Ordinary users should not normally have to import this module.

module Solr.Query.Class
  ( -- * Solr query language
    QuerySYM(..)
  , neg
    -- * Named operators
  , field
  , Solr.Query.Class.and
  , Solr.Query.Class.or
  , Solr.Query.Class.not
  , score
    -- * Re-exports
  , module Solr.Expr.Class
  ) where

import Solr.Expr.Class

import Data.Text (Text)


-- $setup
-- >>> import Solr.Query


-- | Solr query language.
class ExprSYM expr => QuerySYM expr query where
  -- | A default field query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] (defaultField (word "foo") :: Query Expr)
  -- "q=foo"
  defaultField :: expr a -> query expr

  -- | A field query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] ("foo" =: word "bar" :: Query Expr)
  -- "q=foo:bar"
  (=:) :: Text -> expr a -> query expr
  infix 5 =:

  -- | An @AND@ query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] ("foo" =: word "bar" &&: "baz" =: word "qux" :: Query Expr)
  -- "q=(foo:bar AND baz:qux)"
  (&&:) :: query expr -> query expr -> query expr
  infixr 3 &&:

  -- | An @OR@ query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] ("foo" =: word "bar" ||: "baz" =: word "qux" :: Query Expr)
  -- "q=(foo:bar OR baz:qux)"
  (||:) :: query expr -> query expr -> query expr
  infixr 2 ||:

  -- | A @NOT@, @\'!\'@, or @\'-\'@ query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] ("foo" =: word "bar" -: "baz" =: word "qux" :: Query Expr)
  -- "q=(foo:bar NOT baz:qux)"
  (-:) :: query expr -> query expr -> query expr
  infixr 1 -:

  -- | The @\'^=\'@ constant score operator.
  --
  -- This is given right-fixity to reject queries like @q ^= 1 ^= 2@.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] ("foo" =: word "bar" ^=: 3.5 :: Query Expr)
  -- "q=foo:bar^=3.5"
  (^=:) :: query expr -> Float -> query expr
  infixr 4 ^=:

-- | Negate a query.
--
-- ==== __Examples__
--
-- >>> compile [] (neg ("foo" =: word "bar") :: Query Expr)
-- "q=(*:[* TO *] NOT foo:bar)"
neg :: QuerySYM expr query => query expr -> query expr
neg = (-:) ("*" =: star `to` star)

-- | Named version of ('=:').
field :: QuerySYM expr query => Text -> expr a -> query expr
field = (=:)

-- | Named version of ('&&:').
and :: QuerySYM expr query => query expr -> query expr -> query expr
and = (&&:)
infixr 3 `and`

-- | Named version of ('||:').
or :: QuerySYM expr query => query expr -> query expr -> query expr
or = (||:)
infixr 2 `or`

-- | Named version of ('-:').
not :: QuerySYM expr query => query expr -> query expr -> query expr
not = (-:)
infixr 1 `not`

-- | Named version of ('^=:').
score :: QuerySYM expr query => query expr -> Float -> query expr
score = (^=:)
infixr 4 `score`

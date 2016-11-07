-- | This module defines the finally tagless Solr DSL. This style admits
-- multiple interpreters, two of which (lazy 'Data.ByteString.Lazy.ByteString's
-- and an initial encoding) are provided by this library, in the "Solr.Query"
-- and "Solr.Query.Initial" modules, respectively.
--
-- Ordinary users should not normally have to import this module.

module Solr.Internal.Class.Query
  ( -- * Solr query language
    SolrQuerySYM(..)
    -- * Named operators
  , field
  , Solr.Internal.Class.Query.and
  , Solr.Internal.Class.Query.or
  , Solr.Internal.Class.Query.not
  , score
    -- * Re-exports
  , module Solr.Internal.Class.Expr
  ) where

import Solr.Internal.Class.Expr
import Solr.Query.Param

import Data.Text (Text)


-- $setup
-- >>> import Solr.Query


-- | Solr query language.
class SolrExprSYM expr => SolrQuerySYM expr query where
  -- | A default field query.
  --
  -- >>> defaultField (word "foo") :: SolrQuery SolrExpr
  -- q=foo
  defaultField :: expr a -> query expr

  -- | A field query.
  --
  -- >>> "foo" =: word "bar" :: SolrQuery SolrExpr
  -- q=foo:bar
  (=:) :: Text -> expr a -> query expr
  infix 5 =:

  -- | An @AND@ query.
  --
  -- >>> "foo" =: word "bar" &&: "baz" =: word "qux" :: SolrQuery SolrExpr
  -- q=(foo:bar AND baz:qux)
  (&&:) :: query expr -> query expr -> query expr
  infixr 3 &&:

  -- | An @OR@ query.
  --
  -- >>> "foo" =: word "bar" ||: "baz" =: word "qux" :: SolrQuery SolrExpr
  -- q=(foo:bar OR baz:qux)
  (||:) :: query expr -> query expr -> query expr
  infixr 2 ||:

  -- | A @NOT@, @\'!\'@, or @\'-\'@ query.
  --
  -- >>> "foo" =: word "bar" -: "baz" =: word "qux" :: SolrQuery SolrExpr
  -- q=(foo:bar NOT baz:qux)
  (-:) :: query expr -> query expr -> query expr
  infixr 1 -:

  -- | The @\'^=\'@ constant score operator.
  --
  -- This is given right-fixity to reject queries like @q ^= 1 ^= 2@.
  --
  -- >>> "foo" =: word "bar" ^=: 3.5 :: SolrQuery SolrExpr
  -- q=foo:bar^=3.5
  (^=:) :: query expr -> Float -> query expr
  infixr 4 ^=:

  -- | Negate a query.
  --
  -- >>> neg ("foo" =: word "bar") :: SolrQuery SolrExpr
  -- q=-foo:bar
  neg :: query expr -> query expr

  -- | Add local parameters to a query.
  --
  -- >>> params [paramDefaultField "foo"] (defaultField (word "bar")) :: SolrQuery SolrExpr
  -- q={!df=foo}bar
  params :: [Param query] -> query expr -> query expr


-- | Named version of ('=:').
field :: SolrQuerySYM expr query => Text -> expr a -> query expr
field = (=:)

-- | Named version of ('&&:').
and :: SolrQuerySYM expr query => query expr -> query expr -> query expr
and = (&&:)
infixr 3 `and`

-- | Named version of ('||:').
or :: SolrQuerySYM expr query => query expr -> query expr -> query expr
or = (||:)
infixr 2 `or`

-- | Named version of ('-:').
not :: SolrQuerySYM expr query => query expr -> query expr -> query expr
not = (-:)
infixr 1 `not`

-- | Named version of ('^=:').
score :: SolrQuerySYM expr query => query expr -> Float -> query expr
score = (^=:)
infixr 4 `score`

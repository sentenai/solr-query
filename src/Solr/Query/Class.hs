module Solr.Query.Class where

import Solr.Prelude
import Solr.Expr.Class

-- $setup
-- >>> import Solr.Query

-- | A 'Query' is built from the 'ExprSYM' and 'QuerySYM' languages.
type Query = forall expr query. QuerySYM expr query => query expr

-- | The Solr query language.
class (ExprSYM expr, Semigroup (query expr)) => QuerySYM expr query where
  -- | A default field query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] (defaultField (word "foo"))
  -- "q=foo"
  defaultField :: expr a -> query expr

  -- | A field query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar")
  -- "q=foo:bar"
  (=:) :: Text -> expr a -> query expr
  infix 5 =:

  -- | An @AND@ query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar" &&: "baz" =: word "qux")
  -- "q=(foo:bar AND baz:qux)"
  (&&:) :: query expr -> query expr -> query expr
  infixr 3 &&:

  -- | An @OR@ query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar" ||: "baz" =: word "qux")
  -- "q=(foo:bar OR baz:qux)"
  (||:) :: query expr -> query expr -> query expr
  infixr 2 ||:

  -- | A @NOT@, @\'!\'@, or @\'-\'@ query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar" -: "baz" =: word "qux")
  -- "q=(foo:bar NOT baz:qux)"
  (-:) :: query expr -> query expr -> query expr
  infixr 1 -:

  -- | The @\'^=\'@ constant score operator.
  --
  -- This is given right-fixity to reject queries like @q ^= 1 ^= 2@.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar" ^=: 3.5)
  -- "q=foo:bar^=3.5"
  (^=:) :: query expr -> Float -> query expr
  infixr 4 ^=:

-- | Negate a query.
--
-- ==== __Examples__
--
-- >>> compile [] [] (neg ("foo" =: word "bar"))
-- "q=(*:[* TO *] NOT foo:bar)"
neg :: QuerySYM expr query => query expr -> query expr
neg = (-:) ("*" =: star `to` star)

-- | Named version of ('=:').
field :: QuerySYM expr query => Text -> expr a -> query expr
field = (=:)

-- | Named version of ('&&:').
qand :: QuerySYM expr query => query expr -> query expr -> query expr
qand = (&&:)
infixr 3 `qand`

-- | Named version of ('||:').
qor :: QuerySYM expr query => query expr -> query expr -> query expr
qor = (||:)
infixr 2 `qor`

-- | Named version of ('-:').
qnot :: QuerySYM expr query => query expr -> query expr -> query expr
qnot = (-:)
infixr 1 `qnot`

-- | Named version of ('^=:').
score :: QuerySYM expr query => query expr -> Float -> query expr
score = (^=:)
infixr 4 `score`

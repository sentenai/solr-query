module Solr.Query.Lucene.Class where

import Solr.Prelude
import Solr.Expr.Class
import Solr.Query (Query)
import Solr.Query.Param.Local (LocalParam)

-- $setup
-- >>> import Solr.Query.Lucene

-- | A 'LuceneQuery' is a 'Query' built from the 'LuceneQuerySYM' language.
type LuceneQuery = Query LuceneQuerySYM

data instance LocalParam LuceneQuerySYM
  = Df Text
  | OpAnd
  | OpOr
  deriving Show

type LuceneQueryParam = LocalParam LuceneQuerySYM

-- | The @lucene@ query language.
class (ExprSYM expr, Semigroup (query expr)) => LuceneQuerySYM expr query where
  -- | A default field query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] (defaultField (word "foo"))
  -- "q={!type=lucene}foo"
  defaultField :: expr a -> query expr

  -- | A field query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar")
  -- "q={!type=lucene}foo:bar"
  (=:) :: Text -> expr a -> query expr
  infix 5 =:

  -- | An @AND@ query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar" &&: "baz" =: word "qux")
  -- "q={!type=lucene}(foo:bar AND baz:qux)"
  (&&:) :: query expr -> query expr -> query expr
  infixr 3 &&:

  -- | An @OR@ query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar" ||: "baz" =: word "qux")
  -- "q={!type=lucene}(foo:bar OR baz:qux)"
  (||:) :: query expr -> query expr -> query expr
  infixr 2 ||:

  -- | A @NOT@, @\'!\'@, or @\'-\'@ query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar" -: "baz" =: word "qux")
  -- "q={!type=lucene}(foo:bar NOT baz:qux)"
  (-:) :: query expr -> query expr -> query expr
  infixr 1 -:

  -- | The @\'^=\'@ constant score operator.
  --
  -- This is given right-fixity to reject queries like @q ^= 1 ^= 2@.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar" ^=: 3.5)
  -- "q={!type=lucene}foo:bar^=3.5"
  (^=:) :: query expr -> Float -> query expr
  infixr 4 ^=:

-- | Negate a query.
--
-- ==== __Examples__
--
-- >>> compile [] [] (neg ("foo" =: word "bar"))
-- "q={!type=lucene}(*:* NOT foo:bar)"
neg :: LuceneQuerySYM expr query => query expr -> query expr
neg = (-:) ("*" =: word "*")

-- | Named version of ('=:').
field :: LuceneQuerySYM expr query => Text -> expr a -> query expr
field = (=:)

-- | Named version of ('&&:').
qand :: LuceneQuerySYM expr query => query expr -> query expr -> query expr
qand = (&&:)
infixr 3 `qand`

-- | Named version of ('||:').
qor :: LuceneQuerySYM expr query => query expr -> query expr -> query expr
qor = (||:)
infixr 2 `qor`

-- | Named version of ('-:').
qnot :: LuceneQuerySYM expr query => query expr -> query expr -> query expr
qnot = (-:)
infixr 1 `qnot`

-- | Named version of ('^=:').
score :: LuceneQuerySYM expr query => query expr -> Float -> query expr
score = (^=:)
infixr 4 `score`

-- | The @\'df\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [df "foo"] (defaultField (word "bar"))
-- "q={!type=lucene df=foo}bar"
df :: Text -> LuceneQueryParam
df = Df

-- | The @\'op=AND\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [opAnd] (defaultField (word "foo") <> defaultField (word "bar"))
-- "q={!type=lucene q.op=AND}foo bar"
opAnd :: LuceneQueryParam
opAnd = OpAnd

-- | The @\'op=OR\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [opOr] (defaultField (word "foo") <> defaultField (word "bar"))
-- "q={!type=lucene q.op=OR}foo bar"
opOr :: LuceneQueryParam
opOr = OpOr

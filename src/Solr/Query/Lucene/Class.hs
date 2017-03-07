module Solr.Query.Lucene.Class where

import Solr.Prelude
import Solr.Query (Query)
import Solr.Query.Lucene.Expr.Class
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
class Semigroup query => LuceneQuerySYM query where
  -- | A default field query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] (defaultField (word "foo"))
  -- "q={!lucene}foo"
  defaultField :: LuceneExpr ty -> query

  -- | A field query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar")
  -- "q={!lucene}foo:bar"
  (=:) :: Text -> LuceneExpr ty -> query
  infix 5 =:

  -- | An @AND@ query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar" &&: "baz" =: word "qux")
  -- "q={!lucene}(foo:bar AND baz:qux)"
  (&&:) :: query -> query -> query
  infixr 3 &&:

  -- | An @OR@ query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar" ||: "baz" =: word "qux")
  -- "q={!lucene}(foo:bar OR baz:qux)"
  (||:) :: query -> query -> query
  infixr 2 ||:

  -- | A @NOT@, @\'!\'@, or @\'-\'@ query.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar" -: "baz" =: word "qux")
  -- "q={!lucene}(foo:bar NOT baz:qux)"
  (-:) :: query -> query -> query
  infixr 1 -:

  -- | The @\'^=\'@ constant score operator.
  --
  -- This is given right-fixity to reject queries like @q ^= 1 ^= 2@.
  --
  -- ==== __Examples__
  --
  -- >>> compile [] [] ("foo" =: word "bar" ^=: 3.5)
  -- "q={!lucene}foo:bar^=3.5"
  (^=:) :: query -> Float -> query
  infixr 4 ^=:

-- | Negate a query.
--
-- ==== __Examples__
--
-- >>> compile [] [] (neg ("foo" =: word "bar"))
-- "q={!lucene}(*:* NOT foo:bar)"
neg :: LuceneQuerySYM query => query -> query
neg = (-:) ("*" =: word "*")

-- | Named version of ('=:').
field :: LuceneQuerySYM query => Text -> LuceneExpr ty -> query
field = (=:)

-- | Named version of ('&&:').
qand :: LuceneQuerySYM query => query -> query -> query
qand = (&&:)
infixr 3 `qand`

-- | Named version of ('||:').
qor :: LuceneQuerySYM query => query -> query -> query
qor = (||:)
infixr 2 `qor`

-- | Named version of ('-:').
qnot :: LuceneQuerySYM query => query -> query -> query
qnot = (-:)
infixr 1 `qnot`

-- | Named version of ('^=:').
score :: LuceneQuerySYM query => query -> Float -> query
score = (^=:)
infixr 4 `score`

-- | The @\'df\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [df "foo"] (defaultField (word "bar"))
-- "q={!lucene df=foo}bar"
df :: Text -> LuceneQueryParam
df = Df

-- | The @\'op=AND\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [opAnd] (defaultField (word "foo") <> defaultField (word "bar"))
-- "q={!lucene q.op=AND}foo bar"
opAnd :: LuceneQueryParam
opAnd = OpAnd

-- | The @\'op=OR\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [opOr] (defaultField (word "foo") <> defaultField (word "bar"))
-- "q={!lucene q.op=OR}foo bar"
opOr :: LuceneQueryParam
opOr = OpOr

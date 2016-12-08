{-# language CPP #-}

#if MIN_VERSION_base(4,9,0)
{-# options_ghc -fno-warn-redundant-constraints #-}
#endif

module Solr.Query.LocalParam where

import Solr.Expr.Type
import Solr.Prelude

-- $setup
-- >>> import Solr.Query

-- | Local query parameters.
data LocalParam (query :: (ExprTy -> *) -> *)
  = LocalParamCache Bool
  | LocalParamCost  Int
  | LocalParamDf    Text
  | LocalParamOpAnd
  | LocalParamOpOr

deriving instance Show (LocalParam query)

-- | A convenient type alias for a list of 'LocalParam' that all work on
-- top-level queries.
type QueryLocalParams
  = forall query.
    (HasLocalParamDf query, HasLocalParamOp query) => [LocalParam query]

-- | A convenient type alias for a list of 'LocalParam' that all work on
-- filter queries.
type FilterQueryLocalParams
  = forall query.
    (HasLocalParamCache query, HasLocalParamCost query, HasLocalParamDf query,
      HasLocalParamOp query) => [LocalParam query]

-- | The @\'cache\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let filterQuery = "baz" =: gte (int 10)
-- >>> compile [fq [cache False] filterQuery] [] ("foo" =: word "bar")
-- "fq={!cache=false}baz:[10 TO *]&q=foo:bar"
cache :: HasLocalParamCache query => Bool -> LocalParam query
cache = LocalParamCache

-- | The @\'cost\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let filterQuery = "baz" =: gte (int 10)
-- >>> compile [fq [cost 5] filterQuery] [] ("foo" =: word "bar")
-- "fq={!cost=5}baz:[10 TO *]&q=foo:bar"
cost :: HasLocalParamCost query => Int -> LocalParam query
cost = LocalParamCost

-- | The @\'df\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [df "foo"] (defaultField (word "bar"))
-- "q={!df=foo}bar"
df :: HasLocalParamDf query => Text -> LocalParam query
df = LocalParamDf

-- | The @\'op=AND\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [opAnd] (defaultField (word "foo") <> defaultField (word "bar"))
-- "q={!q.op=AND}foo bar"
opAnd :: HasLocalParamOp query => LocalParam query
opAnd = LocalParamOpAnd

-- | The @\'op=OR\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [opOr] (defaultField (word "foo") <> defaultField (word "bar"))
-- "q={!q.op=OR}foo bar"
opOr :: HasLocalParamOp query => LocalParam query
opOr = LocalParamOpOr

class HasLocalParamCache (query :: (ExprTy -> *) -> *)
class HasLocalParamCost  (query :: (ExprTy -> *) -> *)
class HasLocalParamDf    (query :: (ExprTy -> *) -> *)
class HasLocalParamOp    (query :: (ExprTy -> *) -> *)

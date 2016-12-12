{-# language CPP #-}

#if MIN_VERSION_base(4,9,0)
{-# options_ghc -fno-warn-redundant-constraints #-}
#endif

module Solr.Query.LocalParam where

import Solr.Prelude

-- $setup
-- >>> import Solr.Query

data LocalParamTy
  = QueryLocalParam
  | FilterQueryLocalParam

-- | Local query parameters.
data LocalParam :: LocalParamTy -> * where
  LocalParamCache :: Bool -> LocalParam 'FilterQueryLocalParam
  LocalParamCost  :: Int  -> LocalParam 'FilterQueryLocalParam
  LocalParamDf    :: Text -> LocalParam ty
  LocalParamOpAnd ::         LocalParam ty
  LocalParamOpOr  ::         LocalParam ty

deriving instance Show (LocalParam ty)

-- | The @\'cache\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let filterQuery = "baz" =: gte (int 10)
-- >>> compile [fq [cache False] filterQuery] [] ("foo" =: word "bar")
-- "fq={!cache=false}baz:[10 TO *]&q=foo:bar"
cache :: Bool -> LocalParam 'FilterQueryLocalParam
cache = LocalParamCache

-- | The @\'cost\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> let filterQuery = "baz" =: gte (int 10)
-- >>> compile [fq [cost 5] filterQuery] [] ("foo" =: word "bar")
-- "fq={!cost=5}baz:[10 TO *]&q=foo:bar"
cost :: Int -> LocalParam 'FilterQueryLocalParam
cost = LocalParamCost

-- | The @\'df\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [df "foo"] (defaultField (word "bar"))
-- "q={!df=foo}bar"
df :: Text -> LocalParam ty
df = LocalParamDf

-- | The @\'op=AND\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [opAnd] (defaultField (word "foo") <> defaultField (word "bar"))
-- "q={!q.op=AND}foo bar"
opAnd :: LocalParam ty
opAnd = LocalParamOpAnd

-- | The @\'op=OR\'@ local parameter.
--
-- ==== __Examples__
--
-- >>> compile [] [opOr] (defaultField (word "foo") <> defaultField (word "bar"))
-- "q={!q.op=OR}foo bar"
opOr :: LocalParam ty
opOr = LocalParamOpOr

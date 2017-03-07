module Solr.Query.Internal where

import Solr.Prelude
import Solr.Query.Param.Local (LocalParam)

import GHC.Exts (Constraint)

-- | A @'Query' sym@ is a @Solr@ query whose semantics is described by the
-- typeclass @sym@.
type Query (sym :: * -> Constraint) = forall query. sym query => query

class InterpretQuery sym a where
  interpretParams :: [LocalParam sym] -> a
  interpretQuery :: Proxy sym -> Query sym -> a

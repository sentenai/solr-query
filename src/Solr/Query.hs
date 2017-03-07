module Solr.Query where

import Solr.Query.Param.Local (LocalParam)

import Data.Proxy (Proxy)
import GHC.Exts (Constraint)

-- | A @'Query' sym@ is a @Solr@ query whose semantics is described by the
-- typeclass @sym@.
type Query (sym :: * -> Constraint) = forall query. sym query => query

class InterpretQuery sym a where
  interpretParams :: [LocalParam sym] -> a
  interpretQuery :: Proxy sym -> Query sym -> a

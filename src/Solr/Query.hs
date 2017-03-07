module Solr.Query where

import Solr.Expr.Type (ExprTy)
import Solr.Query.Param.Local (LocalParam)

import Data.Proxy (Proxy)
import GHC.Exts (Constraint)

-- | A @'Query' sym@ is a @Solr@ query whose semantics is described by the
-- typeclass @sym@.
type Query (sym :: (ExprTy -> *) -> ((ExprTy -> *) -> *) -> Constraint) =
  forall expr query. sym expr query => query expr

class InterpretQuery sym a where
  interpretParams :: [LocalParam sym] -> a
  interpretQuery :: Proxy sym -> Query sym -> a

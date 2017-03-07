module Solr.Query.Param.Local where

import GHC.Exts (Constraint)

data family LocalParam (sym :: k -> Constraint)

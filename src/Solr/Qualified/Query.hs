-- | This module is an alternative to "Solr.Query" that does not export any
-- operators, and is intended to be imported qualified, because it contains
-- function names that clash with the Prelude.
--
-- > import qualified Solr.Qualified.Query as Solr

module Solr.Qualified.Query
  (
    -- * Re-exports
    module Solr.Qualified.Class
  , module Solr.Query
  ) where

import Solr.Qualified.Class (fuzz, boost, field, and, or, not, score)
import Solr.Query           hiding ((~:), (^:), (=:), (&&:), (||:), (-:), (^=:), (.=))

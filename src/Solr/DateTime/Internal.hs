-- | 'DateTime' internals.

-- This public module exists to expose the DateTime constructors, while still
-- hiding the internal @IsDateTime@ typeclass (because it's "closed").

module Solr.DateTime.Internal
  ( DateTime(..)
  , TruncatedDateTime
  , Year
  , Month
  , Day
  , Hour
  , Minute
  , Second
  , Millisecond
  , IsDateTime
  ) where

import Solr.DateTime.ReallyInternal

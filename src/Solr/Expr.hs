module Solr.Expr
  ( -- * Expression type
    SolrExpr
    -- * Expression construction
  , num
  , true
  , false
  , word
  , wild
  , regex
  , phrase
  , (~:)
  , fuzz
  , fuzzy
  , to
  , incl
  , excl
  , star
  , gt
  , gte
  , lt
  , lte
  , (^:)
  , boost
  ) where

import Solr.Expr.Internal
import Solr.Internal.Class

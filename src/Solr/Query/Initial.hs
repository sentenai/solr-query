{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | An initial encoding of a Solr query. This is an alternative interpretation
-- of the Solr language that is more amenable to parsing from arbitrary user
-- input.

module Solr.Query.Initial
  (
  -- * Query type
    SolrQueryI(..)
  -- * Expression type
  , SolrExprI(..)
  -- * Query compilation
  , solrQueryI
  -- * Untyped ADTs
  , USolrQueryI(..)
  , USolrExprI(..)
  ) where

import Solr.Class
import Solr.Param
import Solr.Type

import Data.Text (Text)

-- | A Solr expression.
data SolrExprI :: SolrType -> * where
  ENum :: Float -> SolrExprI 'TNum
  ETrue :: SolrExprI 'TBool
  EFalse :: SolrExprI 'TBool
  EWord :: Text -> SolrExprI 'TWord
  EWild :: Text -> SolrExprI 'TWild
  ERegex :: Text -> SolrExprI 'TRegex
  EPhrase :: [SolrExprI 'TWord] -> SolrExprI 'TPhrase
  EFuzz :: FuzzableType a => SolrExprI a -> Int -> SolrExprI 'TFuzzed
  ETo :: PrimType a => Boundary (SolrExprI a) -> Boundary (SolrExprI a) -> SolrExprI 'TRange
  EBoost :: BoostableType a => SolrExprI a -> Float -> SolrExprI 'TBoosted

instance SolrExprSYM SolrExprI where
  num    = ENum
  true   = ETrue
  false  = EFalse
  word   = EWord
  wild   = EWild
  regex  = ERegex
  phrase = EPhrase
  (~:)   = EFuzz
  to     = ETo
  (^:)   = EBoost


-- | A Solr query.
data SolrQueryI where
  QDefaultField :: SolrExprI a -> SolrQueryI
  QField :: Text -> SolrExprI a -> SolrQueryI
  QAnd :: SolrQueryI -> SolrQueryI -> SolrQueryI
  QOr :: SolrQueryI -> SolrQueryI -> SolrQueryI
  QNot :: SolrQueryI -> SolrQueryI -> SolrQueryI
  QScore :: SolrQueryI -> Float -> SolrQueryI
  QNeg :: SolrQueryI -> SolrQueryI
  QParams :: [Param SolrQueryI] -> SolrQueryI -> SolrQueryI

instance SolrQuerySYM SolrExprI SolrQueryI where
  data ParamKey SolrQueryI a where
    SolrQueryIDefaultField :: ParamKey SolrQueryI Text
    SolrQueryIOp           :: ParamKey SolrQueryI Text

  defaultField = QDefaultField
  (=:)         = QField
  (&&:)        = QAnd
  (||:)        = QOr
  (-:)         = QNot
  (^=:)        = QScore
  neg          = QNeg
  params       = QParams

instance HasParamDefaultField SolrQueryI where
  paramDefaultField = SolrQueryIDefaultField

instance HasParamOp SolrQueryI where
  paramOp = SolrQueryIOp


-- | Select the 'SolrQueryI' interpreter.
--
-- > solrQueryI = id
solrQueryI :: SolrQueryI -> SolrQueryI
solrQueryI = id


-- | An untyped Solr expression.
data USolrExprI
  = UENum Float
  | UETrue
  | UEFalse
  | UEWord Text
  | UEWild Text
  | UERegex Text
  | UEPhrase [USolrExprI]
  | UEFuzz USolrExprI Int
  | UETo (Boundary USolrExprI) (Boundary USolrExprI)
  | UEBoost USolrExprI Float

-- | An untyped Solr query.
data USolrQueryI
  = UQDefaultField USolrExprI
  | UQField Text USolrExprI
  | UQAnd USolrQueryI USolrQueryI USolrQueryI
  | UQOr USolrQueryI USolrQueryI USolrQueryI
  | UQNot USolrQueryI USolrQueryI USolrQueryI
  | UQScore USolrQueryI Float
  | UQNeg USolrQueryI
  | UQParams {- [Param SolrQueryI] TODO figure this out -} USolrQueryI

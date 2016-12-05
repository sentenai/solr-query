module Solr.Expr.Initial.Typed
  ( -- * Expression type
    Expr(..)
  , typeCheck
  , reinterpret
    -- * Re-exports
  , module Solr.DateTime.Internal
  , module Solr.Expr.Class
  ) where

import Solr.DateTime.Internal
import Solr.DateTime.ReallyInternal
import Solr.Expr.Class
import Solr.Type

import qualified Solr.Expr.Initial.Untyped as Untyped

import Control.Applicative (pure)
import Control.Monad       (forM)
import Data.Int            (Int64)
import Data.Text           (Text)
import Data.Time           (UTCTime)


-- | A typed, initially-encoded Solr expression.
data Expr   :: SolrType -> * where
  EInt      :: Int64  -> Expr 'TNum
  EFloat    :: Double -> Expr 'TNum
  ETrue     :: Expr 'TBool
  EFalse    :: Expr 'TBool
  EWord     :: Text -> Expr 'TWord
  EWild     :: Text -> Expr 'TWild
  ERegex    :: Text -> Expr 'TRegex
  EPhrase   :: [Expr 'TWord] -> Expr 'TPhrase
  EUTCTime  :: UTCTime -> Expr 'TDateTime
  EDateTime :: TruncatedDateTime -> Expr 'TDateTime
  EFuzz     :: Fuzzable a => Expr a -> Int -> Expr ('TFuzzed a)
  ETo       :: Rangeable a => Boundary (Expr a) -> Boundary (Expr a) -> Expr ('TRanged a)
  EBoost    :: Boostable a => Expr a -> Float -> Expr ('TBoosted a)

instance ExprSYM Expr where
  int     = EInt
  float   = EFloat
  true    = ETrue
  false   = EFalse
  word    = EWord
  wild    = EWild
  regex   = ERegex
  phrase  = EPhrase
  (~:)    = EFuzz
  to      = ETo
  (^:)    = EBoost

  datetime t =
    case toDateTime t of
      UTC t'       -> EUTCTime t'
      Truncated t' -> EDateTime t'


-- | Existential wrapper around 'Expr'.
data SomeExpr where
  SomeExpr :: Expr ty -> SomeExpr

-- | Type check an untyped Solr expression. Note the untyped 'Untyped.Expr' on
-- the way in is not the same as the typed 'Expr' on the way out.
--
-- @
-- 'typeCheck' u k =
--   case 'typeCheck'' u of
--     Nothing -> k Nothing
--     Just ('SomeExpr' e) -> k (Just e)
-- @
typeCheck :: Untyped.Expr a -> (forall ty. Maybe (Expr ty) -> r) -> r
typeCheck u k =
  case typeCheck' u of
    Nothing -> k Nothing
    Just (SomeExpr e) -> k (Just e)

-- | Like 'typeCheck', but return an existential type rather than use
-- rank-2 continuation passing style, if you prefer.
typeCheck' :: Untyped.Expr a -> Maybe SomeExpr
typeCheck' u0 =
  case u0 of
    Untyped.EInt n      -> pure (SomeExpr (EInt n))
    Untyped.EFloat n    -> pure (SomeExpr (EFloat n))
    Untyped.ETrue       -> pure (SomeExpr ETrue)
    Untyped.EFalse      -> pure (SomeExpr EFalse)
    Untyped.EWord s     -> pure (SomeExpr (EWord s))
    Untyped.EWild s     -> pure (SomeExpr (EWild s))
    Untyped.ERegex s    -> pure (SomeExpr (ERegex s))
    Untyped.EUTCTime  t -> pure (SomeExpr (EUTCTime t))
    Untyped.EDateTime t -> pure (SomeExpr (EDateTime t))

    Untyped.EPhrase ss0 -> do
      es <- forM ss0 (\s -> do
                       SomeExpr e@(EWord _) <- typeCheck' s
                       pure e)
      pure (SomeExpr (EPhrase es))

    Untyped.EFuzz u n -> do
      SomeExpr e <- typeCheck' u
      case e of
        EWord _   -> pure (SomeExpr (EFuzz e n))
        EPhrase _ -> pure (SomeExpr (EFuzz e n))
        _         -> Nothing

    -- FIXME: Hm, when type checking a [* TO *], do I really have to just pick a
    -- type here? Seems wrong...
    Untyped.ETo Star Star ->
      pure (SomeExpr (ETo (Star :: Boundary (Expr 'TNum)) Star))

    Untyped.ETo Star (Inclusive u)            -> starLeft  Inclusive u
    Untyped.ETo Star (Exclusive u)            -> starLeft  Exclusive u
    Untyped.ETo (Inclusive u) Star            -> starRight Inclusive u
    Untyped.ETo (Exclusive u) Star            -> starRight Exclusive u

    Untyped.ETo (Inclusive u1) (Inclusive u2) -> noStar Inclusive Inclusive u1 u2
    Untyped.ETo (Inclusive u1) (Exclusive u2) -> noStar Inclusive Exclusive u1 u2
    Untyped.ETo (Exclusive u1) (Inclusive u2) -> noStar Exclusive Inclusive u1 u2
    Untyped.ETo (Exclusive u1) (Exclusive u2) -> noStar Exclusive Exclusive u1 u2

    Untyped.EBoost u n -> do
      SomeExpr e <- typeCheck' u
      case e of
        EWord _   -> pure (SomeExpr (EBoost e n))
        EPhrase _ -> pure (SomeExpr (EBoost e n))
        _         -> Nothing

-- Type check a *-to-EXPR
starLeft :: (forall x. x -> Boundary x) -> Untyped.Expr a -> Maybe SomeExpr
starLeft con u = do
  SomeExpr e <- typeCheck' u
  case e of
    EInt _   -> pure (SomeExpr (ETo Star (con e)))
    EFloat _ -> pure (SomeExpr (ETo Star (con e)))
    EWord _  -> pure (SomeExpr (ETo Star (con e)))
    _        -> Nothing

-- Type check a EXPR-to-*
starRight :: (forall x. x -> Boundary x) -> Untyped.Expr a -> Maybe SomeExpr
starRight con u = do
  SomeExpr e <- typeCheck' u
  case e of
    EInt _   -> pure (SomeExpr (ETo (con e) Star))
    EFloat _ -> pure (SomeExpr (ETo (con e) Star))
    EWord _  -> pure (SomeExpr (ETo (con e) Star))
    _        -> Nothing

-- Type check a EXPR-to-EXPR
noStar
  :: (forall x. x -> Boundary x)
  -> (forall x. x -> Boundary x)
  -> Untyped.Expr a
  -> Untyped.Expr a
  -> Maybe SomeExpr
noStar con1 con2 u1 u2 = do
  SomeExpr e1 <- typeCheck' u1
  SomeExpr e2 <- typeCheck' u2
  case (e1, e2) of
    (EInt   _, EInt   _) -> pure (SomeExpr (ETo (con1 e1) (con2 e2)))
    (EInt   _, EFloat _) -> pure (SomeExpr (ETo (con1 e1) (con2 e2)))
    (EFloat _, EInt   _) -> pure (SomeExpr (ETo (con1 e1) (con2 e2)))
    (EFloat _, EFloat _) -> pure (SomeExpr (ETo (con1 e1) (con2 e2)))
    (EWord  _, EWord  _) -> pure (SomeExpr (ETo (con1 e1) (con2 e2)))
    _                    -> Nothing


-- | Reinterpret a Solr expression.
reinterpret :: ExprSYM expr => Expr ty -> expr ty
reinterpret = \case
  EInt n      -> int n
  EFloat n    -> float n
  ETrue       -> true
  EFalse      -> false
  EWord s     -> word s
  EWild s     -> wild s
  ERegex s    -> regex s
  EUTCTime s  -> datetime s
  EDateTime s -> unTruncated s datetime
  EPhrase es  -> phrase (map reinterpret es)
  EFuzz e n   -> reinterpret e ~: n
  ETo e1 e2   -> fmap reinterpret e1 `to` fmap reinterpret e2
  EBoost e n  -> reinterpret e ^: n

unTruncated :: TruncatedDateTime -> (forall a. IsDateTime a => a -> r) -> r
unTruncated (a, Nothing)                                                   k = k a
unTruncated (a, Just (b, Nothing))                                         k = k (a, b)
unTruncated (a, Just (b, Just (c, Nothing)))                               k = k (a, b, c)
unTruncated (a, Just (b, Just (c, Just (d, Nothing))))                     k = k (a, b, c, d)
unTruncated (a, Just (b, Just (c, Just (d, Just (e, Nothing)))))           k = k (a, b, c, d, e)
unTruncated (a, Just (b, Just (c, Just (d, Just (e, Just (f, Nothing)))))) k = k (a, b, c, d, e, f)
unTruncated (a, Just (b, Just (c, Just (d, Just (e, Just (f, Just g))))))  k = k (a, b, c, d, e, f, g)

module Solr.Query.Lucene.Expr.Impl.Initial.Typed where

import Solr.Prelude
import Solr.Query.Lucene.Expr.Class
import Solr.Query.Lucene.Expr.Impl.Initial.Untyped
import Solr.Query.Lucene.Expr.Type

import Data.String (IsString(..))

-- | A typed, initially-encoded @lucene@ expression.
data Expr :: LuceneExprTy -> * where
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
  EFuzz     :: Fuzzable a => Expr a -> Int -> Expr 'TFuzzy
  ETo       :: Rangeable a b => Boundary Expr a -> Boundary Expr b -> Expr 'TRange
  EBoost    :: Boostable a => Expr a -> Float -> Expr 'TBoosted

instance IsString (Expr 'TWord) where
  fromString s = word (pack s)

instance LuceneExprSYM Expr where
  int    = EInt
  float  = EFloat
  true   = ETrue
  false  = EFalse
  word   = EWord
  wild   = EWild
  regex  = ERegex
  phrase = EPhrase
  (~:)   = EFuzz
  to     = ETo
  (^:)   = EBoost

  datetime t =
    case toDateTime t of
      UTC t'       -> EUTCTime t'
      Truncated t' -> EDateTime t'

-- Existential wrapper around 'Expr'.
data SomeExpr where
  SomeExpr :: Expr ty -> SomeExpr

-- Type check a 'UExpr'.
typeCheckE :: UExpr a -> (forall ty. Maybe (Expr ty) -> r) -> r
typeCheckE u k =
  case typeCheckE' u of
    Nothing -> k Nothing
    Just (SomeExpr e) -> k (Just e)

typeCheckE' :: UExpr a -> Maybe SomeExpr
typeCheckE' u0 =
  case u0 of
    UInt n      -> pure (SomeExpr (EInt n))
    UFloat n    -> pure (SomeExpr (EFloat n))
    UTrue       -> pure (SomeExpr ETrue)
    UFalse      -> pure (SomeExpr EFalse)
    UWord s     -> pure (SomeExpr (EWord s))
    UWild s     -> pure (SomeExpr (EWild s))
    URegex s    -> pure (SomeExpr (ERegex s))
    UUTCTime  t -> pure (SomeExpr (EUTCTime t))
    UDateTime t -> pure (SomeExpr (EDateTime t))

    UPhrase ss0 -> do
      es <- forM ss0 (\s -> do
                       SomeExpr e@(EWord _) <- typeCheckE' s
                       pure e)
      pure (SomeExpr (EPhrase es))

    UFuzz u n -> do
      SomeExpr e <- typeCheckE' u
      case e of
        EWord _   -> pure (SomeExpr (EFuzz e n))
        EPhrase _ -> pure (SomeExpr (EFuzz e n))
        _         -> Nothing

    UTo Star Star -> pure (SomeExpr (ETo Star Star))

    UTo Star (Inclusive u)            -> starLeft  Inclusive u
    UTo Star (Exclusive u)            -> starLeft  Exclusive u
    UTo (Inclusive u) Star            -> starRight Inclusive u
    UTo (Exclusive u) Star            -> starRight Exclusive u

    UTo (Inclusive u1) (Inclusive u2) -> noStar Inclusive Inclusive u1 u2
    UTo (Inclusive u1) (Exclusive u2) -> noStar Inclusive Exclusive u1 u2
    UTo (Exclusive u1) (Inclusive u2) -> noStar Exclusive Inclusive u1 u2
    UTo (Exclusive u1) (Exclusive u2) -> noStar Exclusive Exclusive u1 u2

    UBoost u n -> do
      SomeExpr e <- typeCheckE' u
      case e of
        EWord _   -> pure (SomeExpr (EBoost e n))
        EPhrase _ -> pure (SomeExpr (EBoost e n))
        _         -> Nothing
 where
  -- Type check a *-to-EXPR
  starLeft
    :: (forall expr x. expr x -> Boundary expr x) -> UExpr a
    -> Maybe SomeExpr
  starLeft con u = do
    SomeExpr e <- typeCheckE' u
    case e of
      EInt _   -> pure (SomeExpr (ETo Star (con e)))
      EFloat _ -> pure (SomeExpr (ETo Star (con e)))
      EWord _  -> pure (SomeExpr (ETo Star (con e)))
      _        -> Nothing

  -- Type check a EXPR-to-*
  starRight
    :: (forall expr x. expr x -> Boundary expr x) -> UExpr a
    -> Maybe SomeExpr
  starRight con u = do
    SomeExpr e <- typeCheckE' u
    case e of
      EInt _   -> pure (SomeExpr (ETo (con e) Star))
      EFloat _ -> pure (SomeExpr (ETo (con e) Star))
      EWord _  -> pure (SomeExpr (ETo (con e) Star))
      _        -> Nothing

  -- Type check a EXPR-to-EXPR
  noStar
    :: (forall expr x. expr x -> Boundary expr x)
    -> (forall expr x. expr x -> Boundary expr x)
    -> UExpr a
    -> UExpr b
    -> Maybe SomeExpr
  noStar con1 con2 u1 u2 = do
    SomeExpr e1 <- typeCheckE' u1
    SomeExpr e2 <- typeCheckE' u2
    case (e1, e2) of
      (EInt   _, EInt   _) -> pure (SomeExpr (ETo (con1 e1) (con2 e2)))
      (EInt   _, EFloat _) -> pure (SomeExpr (ETo (con1 e1) (con2 e2)))
      (EFloat _, EInt   _) -> pure (SomeExpr (ETo (con1 e1) (con2 e2)))
      (EFloat _, EFloat _) -> pure (SomeExpr (ETo (con1 e1) (con2 e2)))
      (EWord  _, EWord  _) -> pure (SomeExpr (ETo (con1 e1) (con2 e2)))
      _                    -> Nothing

-- Reinterpret a Solr expression.
reinterpretE :: LuceneExprSYM expr => Expr ty -> expr ty
reinterpretE = \case
  EInt n      -> int n
  EFloat n    -> float n
  ETrue       -> true
  EFalse      -> false
  EWord s     -> word s
  EWild s     -> wild s
  ERegex s    -> regex s
  EUTCTime s  -> datetime s
  EDateTime s -> unTruncated s datetime
  EPhrase es  -> phrase (map reinterpretE es)
  EFuzz e n   -> reinterpretE e ~: n
  EBoost e n  -> reinterpretE e ^: n

  ETo e1 e2 ->
    case (e1, e2) of
      (Inclusive e1', Inclusive e2') -> incl (reinterpretE e1') `to` incl (reinterpretE e2')
      (Inclusive e1', Exclusive e2') -> incl (reinterpretE e1') `to` excl (reinterpretE e2')
      (Inclusive e1', Star)          -> incl (reinterpretE e1') `to` star
      (Exclusive e1', Inclusive e2') -> excl (reinterpretE e1') `to` incl (reinterpretE e2')
      (Exclusive e1', Exclusive e2') -> excl (reinterpretE e1') `to` excl (reinterpretE e2')
      (Exclusive e1', Star)          -> excl (reinterpretE e1') `to` star
      (Star,          Inclusive e2') -> star                   `to` incl (reinterpretE e2')
      (Star,          Exclusive e2') -> star                   `to` excl (reinterpretE e2')
      (Star,          Star)          -> star                   `to` star
 where
  unTruncated :: TruncatedDateTime -> (forall a. IsDateTime a => a -> r) -> r
  unTruncated (a, Nothing)                                                   k = k a
  unTruncated (a, Just (b, Nothing))                                         k = k (a, b)
  unTruncated (a, Just (b, Just (c, Nothing)))                               k = k (a, b, c)
  unTruncated (a, Just (b, Just (c, Just (d, Nothing))))                     k = k (a, b, c, d)
  unTruncated (a, Just (b, Just (c, Just (d, Just (e, Nothing)))))           k = k (a, b, c, d, e)
  unTruncated (a, Just (b, Just (c, Just (d, Just (e, Just (f, Nothing)))))) k = k (a, b, c, d, e, f)
  unTruncated (a, Just (b, Just (c, Just (d, Just (e, Just (f, Just g))))))  k = k (a, b, c, d, e, f, g)

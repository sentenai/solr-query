module Solr.Query.Lucene.Expr.Impl.Final where

import Builder
import Solr.Query.Lucene.Expr.Class
import Solr.Query.Lucene.Expr.Type
import Solr.Prelude

import Data.String (IsString(..))

-- 'Builder' interpretation of a 'LuceneExprSYM'.
newtype E (t :: LuceneExprTy) = E { unE :: Builder }

instance IsString (E 'TWord) where
  fromString s = word (pack s)

instance LuceneExprSYM E where
  int n = E (bshow n)

  float n = E (bshow n)

  true = E "true"

  false = E "false"

  word s = E (thaw' s)

  wild s = E (thaw' s)

  regex s = E (char '/' <> thaw' s <> char '/')

  phrase ss = E (dquotes (intersperse ' ' (map unE ss)))

  datetime t =
    case toDateTime t of
      UTC t' -> E (thawStr (formatTime defaultTimeLocale "\"%Y-%m-%dT%H:%M:%S%QZ\"" t'))
      Truncated t' -> E (formatTruncated t')
   where
    formatTruncated :: TruncatedDateTime -> Builder
    formatTruncated =
      go '"' show
        (go '-' fmt
          (go '-' fmt
            (go 'T' fmt
              (go ':' fmt
                (go ':' fmt formatMilli)))))
     where
      go :: Char -> (a -> String) -> (b -> Builder) -> (a, Maybe b) -> Builder
      go c f g (a, b) = char c <> thawStr (f a) <> maybe (char '"') g b

      fmt :: Int -> String
      fmt = printf "%02d"

    -- Format to 5 decimal places
    formatMilli :: Millisecond -> Builder
    formatMilli ml = thawStr (tail (printf "%.5f" (ml / 100))) <> "Z\""

  E e ~: n = E (e <> char '~' <> bshow n)

  to b1 b2 = E (lhs b1 <> " TO " <> rhs b2)
   where
    lhs :: Boundary E a -> Builder
    lhs (Inclusive e) = char '[' <> unE e
    lhs (Exclusive e) = char '{' <> unE e
    lhs Star          = "[*"

    rhs :: Boundary E a -> Builder
    rhs (Inclusive e) = unE e <> char ']'
    rhs (Exclusive e) = unE e <> char '}'
    rhs Star          = "*]"

  E e ^: n = E (e <> char '^' <> bshow n)

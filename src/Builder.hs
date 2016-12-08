module Builder
  ( Builder
  , char
  , bshow
  , intersperse
  , parens
  , dquotes
  , freeze
  , thaw
  , thaw'
  , thawStr
  ) where

import Solr.Prelude

import Data.Text.Lazy.Builder
  (Builder, fromLazyText, fromString, fromText, singleton, toLazyText)

char :: Char -> Builder
char = singleton

bshow :: Show a => a -> Builder
bshow = fromString . show

intersperse :: Char -> [Builder] -> Builder
intersperse c0 = go (singleton c0)
 where
  go _ []     = mempty
  go _ [w]    = w
  go c (w:ws) = w <> c <> go c ws

parens :: Builder -> Builder
parens s = singleton '(' <> s <> singleton ')'

dquotes :: Builder -> Builder
dquotes s = singleton '"' <> s <> singleton '"'

freeze :: Builder -> LText
freeze = toLazyText

thaw :: LText -> Builder
thaw = fromLazyText

thaw' :: Text -> Builder
thaw' = fromText

thawStr :: String -> Builder
thawStr = fromString

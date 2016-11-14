module Builder
  ( Builder
  , LText
  , char
  , bshow
  , spaces
  , parens
  , dquotes
  , freeze
  , thaw
  , thaw'
  , thawStr
  ) where

import Data.Semigroup         (Semigroup(..))
import Data.Text              (Text)
import Data.Text.Lazy.Builder
  (Builder, fromLazyText, fromString, fromText, singleton, toLazyText)

import qualified Data.Text.Lazy as LText

type LText = LText.Text

char :: Char -> Builder
char = singleton

bshow :: Show a => a -> Builder
bshow = fromString . show

spaces :: [Builder] -> Builder
spaces [] = ""
spaces [w] = w
spaces (w:ws) = w <> singleton ' ' <> spaces ws

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
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Builder
  ( module B
  , Builder.show
  , spaces
  ) where

-- Data.ByteString.Lazy.Builder was renamed to Data.ByteString.Builder in 0.10.2.0
#if MIN_VERSION_bytestring(0,10,2)
import Data.ByteString.Builder as B
#else
import Data.ByteString.Lazy.Builder as B
#endif

import Data.Semigroup (Semigroup(..))

import qualified Text.Show.ByteString

show :: Text.Show.ByteString.Show a => a -> B.Builder
show = B.lazyByteString . Text.Show.ByteString.show

spaces :: [B.Builder] -> B.Builder
spaces [] = ""
spaces [w] = w
spaces (w:ws) = w <> " " <> spaces ws

{-# language CPP #-}

module Solr.Prelude
  ( LText
  , defaultTimeLocale
  , module Control.Monad
  , module Data.Coerce
  , module Data.Int
  , module Data.Function
  , module Data.Monoid
  , module Data.Proxy
  , module Data.Semigroup
  , module Data.Text
  , module Data.Time
  , module Text.Printf
  , module Prelude.Compat
  ) where

import Control.Monad (forM)
import Data.Coerce
import Data.Int
import Data.Function
import Data.Monoid (mempty)
import Data.Proxy
import Data.Text (Text, pack)
import Data.Time (UTCTime, formatTime)
import Data.Semigroup (Semigroup(..))
import Prelude.Compat
import Text.Printf (printf)

#if MIN_VERSION_time(1,5,0)
import Data.Time      (defaultTimeLocale)
#else
import System.Locale  (defaultTimeLocale)
#endif


import qualified Data.Text.Lazy as LText

type LText = LText.Text

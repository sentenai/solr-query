-- | https://lucene.apache.org/core/6_5_0/queryparser/org/apache/lucene/queryparser/classic/package-summary.html#Escaping_Special_Characters

module Solr.Query.Escape
  ( escapeWord
  , escapeWild
  ) where

import Solr.Prelude

import Builder

import Data.Char (isSpace)

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText

-- | Escape each of
--
-- @
-- + - & | ! ( ) { } [ ] ^ " ~ * ? : \\ /
-- @
--
-- plus whitespace characters.
escapeWord :: Text -> Text
escapeWord = escape "+-&|!(){}[]^\"~*?:\\/"

-- | Escape each of
--
-- @
-- + - & | ! ( ) { } [ ] ^ " ~ : \\ /
-- @
--
-- plus whitespace characters.
escapeWild :: Text -> Text
escapeWild = escape "+-&|!(){}[]^\"~:\\/"

escape :: [Char] -> Text -> Text
escape special = LText.toStrict . freeze . mconcat . map esc . Text.unpack
 where
  esc :: Char -> Builder
  esc c
    | isSpace c || elem c special = char '\\' <> char c
    | otherwise = char c

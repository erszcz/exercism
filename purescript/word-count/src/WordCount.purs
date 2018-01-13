module WordCount
  ( wordCount
  ) where

import Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable as F
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RF

wordCount :: String -> SM.StrMap Int
wordCount sentence = case mkRe of
    Left _ ->
      SM.empty
    Right re ->
      case Regex.match re $ String.toLower sentence of
           Nothing -> SM.empty
           Just matches -> count $ A.catMaybes matches
  where count words = F.foldl countOne SM.empty words
        countOne counts word = SM.alter alterOne word counts
        alterOne Nothing = Just 1
        alterOne (Just n) = Just (n + 1)

mkRe = Regex.regex "(\\w[\\w']*\\w|\\d+)" RF.global

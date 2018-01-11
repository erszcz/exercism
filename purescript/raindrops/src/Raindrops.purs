module Raindrops
  ( raindrops
  ) where

import Prelude
import Data.Array (fold, mapMaybe)
import Data.Maybe (Maybe(..))

raindrops :: Int -> String
raindrops n = case factors n of
    [] -> show n
    fs -> fold $ mapMaybe repr fs
  where
    repr i = case i of
      3 -> Just "Pling"
      5 -> Just "Plang"
      7 -> Just "Plong"
      _ -> Nothing

factors :: Int -> Array Int
factors n =
  (if n `mod` 3 == 0 then [3] else []) <>
  (if n `mod` 5 == 0 then [5] else []) <>
  (if n `mod` 7 == 0 then [7] else [])

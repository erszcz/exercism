module Raindrops
  ( raindrops
  ) where

import Prelude
import Data.String (joinWith)

raindrops :: Int -> String
raindrops n = case factors n of
    [] -> show n
    fs -> joinWith "" $ map repr fs
  where
    repr i = case i of
      3 -> "Pling"
      5 -> "Plang"
      _ -> "Plong"

factors n =
  (if n `mod` 3 == 0 then [3] else []) <>
  (if n `mod` 5 == 0 then [5] else []) <>
  (if n `mod` 7 == 0 then [7] else [])

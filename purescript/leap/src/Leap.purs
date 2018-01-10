module Leap where

import Prelude (mod)

isLeapYear :: Int -> Boolean
isLeapYear year =
  case [year `mod` 4, year `mod` 100, year `mod` 400] of
    [0, _, 0] -> true
    [0, 0, _] -> false
    [0, _, _] -> true
    _ -> false

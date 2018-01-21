module AllYourBase
  {--( rebase--}
  {--)--}
  where

import Prelude

import Data.Foldable (foldr)
import Data.Array (fromFoldable)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Int (fromNumber, toNumber)
import Math (pow)

rebase :: Int -> Int -> Array Int -> Maybe (Array Int)
rebase from to number | from <= 1 || to <= 1 || number == [] =
  Nothing
rebase from to number =
  fromFoldable <$> (fromInt to) <$> (toInt from number)

toInt :: Int -> Array Int -> Maybe Int
toInt base number =
  let {total, p} = foldr go {total: 0.0, p: 0.0} number
   in fromNumber total
  where base' = toNumber base
        go i {total, p} = {total: total + (toNumber i) * (pow base' p), p: p + 1.0}

fromInt :: Int -> Int -> List Int
fromInt base number = fromInt' number Nil
  where fromInt' number acc | number < base = (number : acc)
        fromInt' number acc = fromInt' (number `div` base) (number `mod` base : acc)

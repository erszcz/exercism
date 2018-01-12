module Hamming
  ( distance
  ) where

import Prelude
import Data.Array  ( foldl, zip )
import Data.Maybe  ( Maybe(..) )
import Data.String ( length, toCharArray )
import Data.Tuple  ( Tuple(..) )

distance :: String -> String -> Maybe Int
distance s t | length s /= length t = Nothing
distance s t = Just $ foldl cmpOne 0 $ zip (toCharArray s) (toCharArray t)
  where cmpOne acc (Tuple a b) | a == b = acc
        cmpOne acc (Tuple _ _) = acc + 1

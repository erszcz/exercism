module Pangram
  ( isPangram
  ) where

import Prelude
import Data.Array as A
import Data.Char as C
import Data.Set as Set
import Data.String as String

isPangram s = Set.isEmpty $ A.foldl removeOne alphabet (String.toCharArray s)
  where removeOne leftChars char = Set.delete (C.toLower char) leftChars
        alphabet = Set.fromFoldable $ map C.fromCharCode $ A.range a z
        a = C.toCharCode 'a'
        z = C.toCharCode 'z'

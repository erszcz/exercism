module ScrabbleScore
  ( scoreWord
  ) where

import Prelude
import Data.Array as A
import Data.Char as C
import Data.Foldable as F
import Data.Set as Set
import Data.String as String
import Data.Maybe ( Maybe(..) )

scoreWord :: String -> Int
scoreWord word = F.sum $ A.mapMaybe scoreLetter (String.toCharArray word)

scoreLetter :: Char -> Maybe Int
scoreLetter l = scoreLetter' $ C.toLower l
  where scoreLetter' l'
          | Set.member l' class1  = Just  1
          | Set.member l' class2  = Just  2
          | Set.member l' class3  = Just  3
          | Set.member l' class4  = Just  4
          | Set.member l' class5  = Just  5
          | Set.member l' class8  = Just  8
          | Set.member l' class10 = Just 10
          | otherwise             = Nothing

class1  = classN "aeioulnrst"
class2  = classN "dg"
class3  = classN "bcmp"
class4  = classN "fhvwy"
class5  = classN "k"
class8  = classN "jx"
class10 = classN "qz"

classN :: String -> Set.Set Char
classN chars = Set.fromFoldable $ String.toCharArray chars

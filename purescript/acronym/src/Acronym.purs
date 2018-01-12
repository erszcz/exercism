module Acronym
  ( abbreviate
  ) where

import Prelude
import Data.Array as A
import Data.Char as C
import Data.Char.Unicode as U
import Data.Maybe ( Maybe(..) )
import Data.String as String
import Data.Tuple ( Tuple(..) )

abbreviate string =
  let (Tuple abbr _) = A.foldl step (Tuple [] []) $ String.toCharArray string
   in String.toUpper $ String.fromCharArray abbr

step (Tuple abbr last) c
  | U.isLetter c && last == [] =
	  Tuple (A.snoc abbr c) [c]
  | U.isLower c =
	  Tuple abbr [c]
  | U.isSpace c =
	  Tuple abbr []
  | U.isUpper c && last /= [] =
	  Tuple (A.snoc abbr c) [c]
  | otherwise =
	  Tuple abbr []

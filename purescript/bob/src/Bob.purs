module Bob
  ( hey
  ) where

import Prelude
import Data.Array (last)
import Data.String (toCharArray)
import Data.Maybe ( Maybe(..) )

data Reaction = Sure
              | ChillOut
              | Fine
              | Whatever

instance showReaction :: Show Reaction where
    show Sure     = "Sure."
    show ChillOut = "Whoa, chill out!"
    show Fine     = "Fine. Be that way!"
    show Whatever = "Whatever."

hey :: String -> String
hey sentence = show $ reactionTo sentence

reactionTo :: String -> Reaction
reactionTo sentence =
  case last $ toCharArray sentence of
    Nothing  -> Fine
    Just '?' -> Sure
    Just '!' -> ChillOut
    Just _   -> Whatever

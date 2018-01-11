module Bob
  ( hey
  ) where

import Prelude
import Data.Array (foldl, last)
import Data.Char.Unicode
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
reactionTo s
  | isSilence s  = Fine
  | isYell s     = ChillOut
  | isQuestion s = Sure
  | otherwise    = Whatever

isSilence s = foldl isSpace true $ toCharArray s
  where isSpace acc c = case generalCategory c of
          Just Space -> acc
          Just Control -> acc
          _ -> false

isQuestion s
  | last (toCharArray s) == Just '?' = true
  | otherwise = false

isYell s =
  case foldl step { lower: 0, upper: 0 } $ toCharArray s of
    { lower: 0, upper: n } | n > 0 -> true
    _ -> false
  where step { lower: l, upper: u } c = case generalCategory c of
          Just LowercaseLetter -> { lower: l + 1, upper: u }
          Just UppercaseLetter -> { lower: l,     upper: u + 1 }
          _                    -> { lower: l,     upper: u }

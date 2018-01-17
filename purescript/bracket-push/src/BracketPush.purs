module BracketPush
  ( isPaired
  ) where

import Prelude

import Data.List (List(..), (:), fromFoldable)
import Data.String (toCharArray)

isPaired expr = isPaired' (fromFoldable $ toCharArray expr) Nil
  where
    isPaired' Nil Nil = true
    isPaired' Nil _ = false
    isPaired' (']':_) ( s :stack) | s /= '[' = false
    isPaired' ('}':_) ( b :stack) | b /= '{' = false
    isPaired' (')':_) ( p :stack) | p /= '(' = false
    isPaired' (']':e) ('[':stack) = isPaired' e stack
    isPaired' ('}':e) ('{':stack) = isPaired' e stack
    isPaired' (')':e) ('(':stack) = isPaired' e stack
    isPaired' ('[':e) stack = isPaired' e ('[':stack)
    isPaired' ('{':e) stack = isPaired' e ('{':stack)
    isPaired' ('(':e) stack = isPaired' e ('(':stack)
    isPaired' ( c :e) stack = isPaired' e stack

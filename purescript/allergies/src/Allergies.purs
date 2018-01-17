module Allergies
  ( allergicTo
  , list
  ) where

import Prelude

import Data.Array (mapMaybe)
import Data.Int.Bits ((.&.))
import Data.Maybe (Maybe(..))

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats

allergenScore :: Allergen → Int
allergenScore al = case al of
  Eggs          ->   1
  Peanuts       ->   2
  Shellfish     ->   4
  Strawberries  ->   8
  Tomatoes      ->  16
  Chocolate     ->  32
  Pollen        ->  64
  Cats          -> 128

allergenFromString :: String -> Maybe Allergen
allergenFromString al = case al of
  "eggs"         -> pure Eggs
  "peanuts"      -> pure Peanuts
  "shellfish"    -> pure Shellfish
  "strawberries" -> pure Strawberries
  "tomatoes"     -> pure Tomatoes
  "chocolate"    -> pure Chocolate
  "pollen"       -> pure Pollen
  "cats"         -> pure Cats
  _              -> Nothing

instance showAllergen :: Show Allergen where
  show al = case al of
                 Eggs          -> "eggs"
                 Peanuts       -> "peanuts"
                 Shellfish     -> "shellfish"
                 Strawberries  -> "strawberries"
                 Tomatoes      -> "tomatoes"
                 Chocolate     -> "chocolate"
                 Pollen        -> "pollen"
                 Cats          -> "cats"

allergicTo :: Int -> String -> Boolean
allergicTo 0 _ = false
allergicTo score al = case (score .&. _) <$> f1 al of
  Just i | i > 0 -> true
  _ -> false
  
f1 :: String → Maybe Int
f1 al = allergenScore <$> allergenFromString al

list :: Int -> Array String
list 0     = []
list score =
 map show $ mapMaybe (\s -> if score .&. (allergenScore s) > 0
                              then Just s
                              else Nothing) $ allergens
 where
  allergens =
    [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats]

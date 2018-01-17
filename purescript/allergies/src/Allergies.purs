module Allergies
  ( allergicTo
  , list
  ) where

import Prelude

allergicTo :: forall a b. a -> b -> Boolean
allergicTo _ _ = false

list :: forall a. a -> Array String
list _ = []

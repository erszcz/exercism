module Accumulate
  ( accumulate
  ) where

import Data.List ( List(..) )

accumulate :: forall a b. (a -> b) -> List a -> List b
accumulate __ Nil = Nil
accumulate op (Cons h t) = Cons (op h) (accumulate op t)

module Triangle
  ( Triangle(Equilateral, Isosceles, Scalene)
  , triangleKind
  ) where

import Data.Either ( Either(..) )

data Triangle = Equilateral
              | Isosceles
              | Scalene

triangleKind _ _ _ = Right Equilateral

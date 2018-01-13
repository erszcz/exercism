module Triangle
  ( Triangle(Equilateral, Isosceles, Scalene)
  , triangleKind
  ) where

import Prelude

import Data.Either (Either(..))

data Triangle = Equilateral
              | Isosceles
              | Scalene

instance eqTriangle :: Eq Triangle where
  eq Equilateral Equilateral = true
  eq Isosceles Isosceles = true
  eq Scalene Scalene = true
  eq _ _ = false

instance showTriangle :: Show Triangle where
  show Equilateral = "Equilateral"
  show Isosceles = "Isosceles"
  show Scalene = "Scalene"

triangleKind :: Int -> Int -> Int -> Either String Triangle
triangleKind a b c | isEquilateral a b c = Right Equilateral
triangleKind a b c | isIsosceles a b c = Right Isosceles
triangleKind a b c | isTriangle a b c = Right Scalene
triangleKind a b c | allSidesPositive a b c = Left "Violates inequality"
triangleKind _ _ _ = Left "Invalid lengths"

isEquilateral a b c | a == b && b == c = isTriangle a b c
isEquilateral _ _ _                    = false

isIsosceles a b c | a == b || b == c || a == c = isTriangle a b c
isIsosceles _ _ _                              = false

isTriangle a b c | allSidesPositive a b c && triangleInequality a b c = true
isTriangle _ _ _ = false

allSidesPositive a b c | a > 0 && b > 0 && c > 0 = true
allSidesPositive _ _ _                           = false

triangleInequality a b c | a + b >= c && a + c >= b && b + c >= a = true
triangleInequality _ _ _                                          = false

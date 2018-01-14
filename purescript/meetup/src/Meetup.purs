module Meetup
  {--( meetup--}
  {--, Week(..)--}
  {--) --}
  where

import Data.Date
import Partial.Unsafe
import Prelude

import Control.Apply (lift2)
import Data.Array as A
import Data.Enum (fromEnum, toEnum)
import Data.Foldable as F
import Data.Maybe (Maybe(..))

data Week = First | Second | Third | Fourth | Last | Teenth

meetup :: Year -> Month -> Week -> Weekday -> Maybe Date
meetup year month week day = unsafePartial meetup' (toEnum 2013) (toEnum 5) (toEnum 13)
  where meetup' :: Partial => Maybe Year -> Maybe Month -> Maybe Day -> Maybe Date
        meetup' (Just y) (Just m) (Just d) =
          canonicalDate y m <$> toEnum (dayteenth Monday week Monday)

dayteenth :: Weekday -> Week -> Weekday -> Int
dayteenth monthFirst w d = F.sum [fromEnum monthFirst, weekOffset w, fromEnum d]

monthWeekdays :: Year -> Month -> Array Weekday
monthWeekdays year month =
  -- We omit the fact that different months have different numbers of days.
  A.catMaybes (map toEnum weekdayNumbers)
  where
    weekdayNumbers = map (\i -> (i + firstWeekdayOffset) `mod` 7 + 1) (A.range 0 30)
    firstWeekdayOffset = fromEnum (monthFirstWeekday year month) - 1

monthFirstWeekday :: Year -> Month -> Weekday
monthFirstWeekday year month = weekday $ canonicalDate year month $ unsafePartial firstDay
  where firstDay :: Partial => Day
        firstDay = case toEnum 1 of
                        Just day -> day

weekOffset w =
  case w of
       First -> 0
       Second -> 7
       Third -> 14
       Fourth -> 21
       Last -> 28
       Teenth -> 12

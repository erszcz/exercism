module Meetup
  {--( meetup--}
  {--, Week(..)--}
  {--) --}
  where

import Data.Date
import Partial.Unsafe
import Prelude

import Control.Apply (lift3)
import Data.Array as A
import Data.Enum (fromEnum, toEnum)
import Data.Foldable as F
import Data.Maybe (Maybe(..))

data Week = First | Second | Third | Fourth | Last | Teenth

meetup :: Year -> Month -> Week -> Weekday -> Maybe Date
meetup year month week weekday =
  lift3 canonicalDate (pure year) (pure month) (toEnum day)
  where
    findDay acc@{ week: 0 } w = acc
    findDay { week: n, day: d } w | w == weekday = { week: n - 1, day: d + 1 }
    findDay { week: n, day: d } w = { week: n, day: d + 1 }
    mFirstWeekday = monthFirstWeekday year month
    weekNumber' = weekNumber year month mFirstWeekday week weekday
    { week: _, day: day } =
      F.foldl findDay { week: weekNumber', day: 0 } $ monthWeekdays mFirstWeekday

monthWeekdays :: Weekday -> Array Weekday
monthWeekdays mFirstWeekday =
  -- We omit the fact that different months have different numbers of days.
  A.catMaybes (map toEnum $ weekdayNumbers)
  where weekdayNumbers = map (integerToWeekdayNumber mFirstWeekday) (A.range 0 30)

integerToWeekdayNumber :: Weekday -> Int -> Int
integerToWeekdayNumber mFirstWeekday i = (i + firstWeekdayOffset) `mod` 7 + 1
  where firstWeekdayOffset = fromEnum mFirstWeekday - 1

monthFirstWeekday :: Year -> Month -> Weekday
monthFirstWeekday year month = weekday $ canonicalDate year month $ unsafePartial firstDay
  where firstDay :: Partial => Day
        firstDay = case toEnum 1 of
                        Just day -> day

weekNumber :: Year -> Month -> Weekday -> Week -> Weekday -> Int
weekNumber year month mFirstWeekday whichWeek weekday =
  case whichWeek of
    First  -> 1
    Second -> 2
    Third  -> 3
    Fourth -> 4
    Last   -> if (isLeapYear year || month /= February)
              && (A.elem weekday $ A.drop 28 $ monthWeekdays mFirstWeekday)
                then 5
                else 4
    Teenth -> 1

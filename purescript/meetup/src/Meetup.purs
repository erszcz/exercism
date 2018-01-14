module Meetup
  {--( meetup--}
  {--, Week(..)--}
  {--) --}
  where

import Data.Date
import Debug.Trace
import Partial.Unsafe
import Prelude

import Control.Apply (lift2, lift3)
import Data.Array as A
import Data.Enum (fromEnum, toEnum)
import Data.Foldable as F
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

data Week = First | Second | Third | Fourth | Last | Teenth

meetup :: Year -> Month -> Week -> Weekday -> Maybe Date
meetup year month week weekday =
  lift3 canonicalDate (pure year) (pure month) (toEnum day)
  where
    findDay acc@{ week: 0 } w = acc
    findDay { week: n, day: d, teenth: t@Teenth } w | w == weekday && d >= 12 && d < 19 = { week: 0, day: d + 1, teenth: t }
    findDay { week: n, day: d, teenth: t } w | w == weekday = { week: n - 1, day: d + 1, teenth: t }
    findDay { week: n, day: d, teenth: t } w = { week: n, day: d + 1, teenth: t }
    mFirstWeekday = monthFirstWeekday year month
    weekNumber' = weekNumber year month mFirstWeekday week weekday
    { week: _, day: day, teenth: _ } =
      F.foldl findDay { week: weekNumber', day: 0, teenth: week } $ monthWeekdays mFirstWeekday

monthWeekdays :: Weekday -> Array Weekday
monthWeekdays mFirstWeekday =
  -- We omit the fact that different months have different numbers of days.
  A.catMaybes (map toEnum $ weekdayNumbers)
  where weekdayNumbers = map (integerToWeekdayNumber mFirstWeekday) (A.range 1 31)

integerToWeekdayNumber :: Weekday -> Int -> Int
integerToWeekdayNumber mFirstWeekday i = (i - 1 + firstWeekdayOffset) `mod` 7 + 1
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
    -- This probably still breaks in case of 30 day long months
    -- and the fact that `monthWeekdays` returns 31 days.
    -- A test for 31st of a month would pass the check below and give 5,
    -- but canonicalDate would return the first day of the next month.
    Last   -> if (isLeapYear year || month /= February)
              && (A.elem weekday $ A.drop 28 $ monthWeekdays mFirstWeekday)
                then 5
                else 4
    Teenth -> 100

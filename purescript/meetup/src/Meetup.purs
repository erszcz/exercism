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
import Data.Enum (class Enum, fromEnum, succ, toEnum)
import Data.Foldable as F
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.Traversable as T
import Data.Tuple (Tuple(..))
import Math (cos)

data Week = First | Second | Third | Fourth | Last | Teenth

data DayInfo = DayInfo
  { no :: Int,
    week :: Int,
    weekday :: Maybe Weekday,
    teenth :: Boolean
  }

derive instance genericTriangle :: Generic DayInfo
instance showDayInfo :: Show DayInfo where
  show = gShow 

dayInfoFromWeekday :: Weekday → DayInfo
dayInfoFromWeekday weekday =
  DayInfo { no: 1, week: 1, weekday: Just weekday, teenth: false }

dayInfo :: Int → Int → Maybe Weekday → Boolean → DayInfo
dayInfo no week weekday teenth =
  DayInfo { no: no, week: week, weekday: weekday, teenth: teenth }

next :: DayInfo -> DayInfo
next (DayInfo { no, week, weekday, teenth }) =
  dayInfo (no + 1)
          (no `div` 7 + 1)
          (case succ weekday of
             Nothing -> Just Monday
             Just wd -> wd)  
          (if no >= 12 && no < 19
              then true
              else false)

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

monthDaysInfo :: Weekday → Array DayInfo
monthDaysInfo mFirstWeekday =
  let initial = dayInfoFromWeekday mFirstWeekday
   in A.cons initial $ T.scanl (\di _ -> next di) initial (A.range 1 30)

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

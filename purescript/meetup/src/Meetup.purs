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
    { week: _, day: day } =
      F.foldl findDay { week: weekNumber week, day: 0 } $ monthWeekdays year month

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

weekNumber w =
  case w of
    First  -> 1
    Second -> 2
    Third  -> 3
    Fourth -> 4
    Last   -> 5
    -- TODO
    Teenth -> 1

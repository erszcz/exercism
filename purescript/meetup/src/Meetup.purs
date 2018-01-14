module Meetup
  {--( meetup--}
  {--, Week(..)--}
  {--) --}
  where

import Prelude

import Control.Apply (lift3)
import Data.Array as A
import Data.Date
import Data.Enum (succ, toEnum)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.Traversable as T
import Partial.Unsafe

data Week = First | Second | Third | Fourth | Last | Teenth

data DayInfo = DayInfo
  { no :: Int,
    week :: Int,
    weekday :: Maybe Weekday,
    teenth :: Boolean
  }

meetup :: Year -> Month -> Week -> Weekday -> Maybe Date
meetup year month week weekday =
  lift3 canonicalDate (pure year) (pure month) (join $ toEnum <$> maybeDay)
  where
    days = eligibleDays year month week weekday
    maybeDay = dayInfoNo <$> (A.last days)

dayInfoNo :: DayInfo → Int
dayInfoNo (DayInfo { no }) = no

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

eligibleDays :: Year → Month → Week → Weekday → Array DayInfo
eligibleDays year month week weekday =
  A.filter filterDay mInfo
  where
    mFirstWeekday = monthFirstWeekday year month
    mInfo = monthDaysInfo mFirstWeekday
    filterDay (DayInfo { no: no, week: wk, weekday: wd, teenth: teenth }) =
      case week of
        First  -> wk == 1 && wd == (pure weekday)
        Second -> wk == 2 && wd == (pure weekday)
        Third  -> wk == 3 && wd == (pure weekday)
        Fourth -> wk == 4 && wd == (pure weekday)
        Last   -> wd == (pure weekday) 
                  && ( ((isLeapYear year || month /= February) && wk == 5)
                       || wk == 4 )
        Teenth -> teenth  && wd == (pure weekday)

monthDaysInfo :: Weekday → Array DayInfo
monthDaysInfo mFirstWeekday =
  let initial = dayInfoFromWeekday mFirstWeekday
   in A.cons initial $ T.scanl (\di _ -> next di) initial (A.range 1 30)

monthFirstWeekday :: Year -> Month -> Weekday
monthFirstWeekday year month = weekday $ canonicalDate year month $ unsafePartial firstDay
  where firstDay :: Partial => Day
        firstDay = case toEnum 1 of
                        Just day -> day

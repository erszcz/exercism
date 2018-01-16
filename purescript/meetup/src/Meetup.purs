module Meetup
  ( meetup
  , Week(..)
  )
  where

import Prelude

import Data.Array ((..))
import Data.Array as A
import Data.Date
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (Maybe)

data Week = First | Second | Third | Fourth | Last | Teenth

meetup :: Year -> Month -> Week -> Weekday -> Maybe Date
meetup y m w wd =
  A.find (\cd -> weekday cd == wd) $ canonicalDate y m <$> A.catMaybes (toEnum <$> days)
  where
    daysInMonth = fromEnum $ lastDayOfMonth y m
    days = case w of
                First  -> 1  .. 7
                Second -> 8  .. 14
                Third  -> 15 .. 21
                Fourth -> 22 .. 28
                Teenth -> 13 .. 19
                Last   -> (daysInMonth - 6) .. daysInMonth

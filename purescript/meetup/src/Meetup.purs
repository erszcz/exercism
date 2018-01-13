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

data Week = First
          | Second
          | Third
          | Fourth
          | Last
          | Teenth

meetup :: Year -> Month -> Week -> Weekday -> Maybe Date
meetup year month week day = unsafePartial meetup' (toEnum 2013) (toEnum 5) (toEnum 13)
  where meetup' :: Partial => Maybe Year -> Maybe Month -> Maybe Day -> Maybe Date
        meetup' (Just y) (Just m) (Just d) =
          canonicalDate y m <$> toEnum (dayteenth Monday week d)

{--meetup :: Year -> Month -> Week -> Weekday -> Maybe Date--}
{--meetup year month week day = canonicalDate year month <$> dayteenth (firstWeekday year month) week day--}

dayteenth :: Weekday -> Week -> Day -> Int
{--dayteenth i w d = (+) <$> (fromEnum <$> i) <@> (fromEnum d)--}
dayteenth monthFirst w d = F.sum [fromEnum monthFirst, weekOffset w, fromEnum d]

  {--<@> (weekOffset w)--}

firstWeekday year month = weekday <$> canonicalDate year month <$> (toEnum 1)

weekOffset w =
  case w of
       First -> 0
       Second -> 7
       Third -> 14
       Fourth -> 21
       Last -> 28
       Teenth -> 12

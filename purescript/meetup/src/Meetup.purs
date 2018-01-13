module Meetup
  ( meetup
  , Week(..)
  ) where

import Prelude
import Data.Date
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (Maybe(..))
import Partial.Unsafe

data Week = First
          | Second
          | Third
          | Fourth
          | Last
          | Teenth

{--meetup :: Year -> Month -> Week -> Weekday -> Maybe Date--}
{--meetup year month week day = Just $ unsafePartial meetup' (toEnum 2013) (toEnum 5) (toEnum 13)--}
{--  where meetup' :: Partial => Maybe Year -> Maybe Month -> Maybe Day -> Date--}
{--        meetup' (Just y) (Just m) (Just d) = canonicalDate y m d--}

meetup :: Year -> Month -> Week -> Weekday -> Maybe Date
meetup year month week day = canonicalDate year month <$> dayteenth week day
  where dayteenth w d = toEnum $ (weekOffset w) + (fromEnum d)
        weekOffset w = case w of
                            First -> 0
                            Second -> 7
                            Third -> 14
                            Fourth -> 21
                            Last -> 28
                            Teenth -> 12
                            Teenth -> 12

module Data.Time.Calendar.WeekDate


import Data.Vect
import Data.Fin

import Data.Time.Calendar.Types
import Data.Time.Calendar.Days
import Data.Time.Calendar.OrdinalDate

%default total

-- ---------------------------------------------------------------------------

public export
data DayOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

export
dayOfWeeks : Vect 7 DayOfWeek
dayOfWeeks = [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]

public export Cast (Fin 7) DayOfWeek where cast x = index x dayOfWeeks
public export
Cast DayOfWeek Int where
  cast Sunday = 0
  cast Monday = 1
  cast Tuesday = 2
  cast Wednesday = 3
  cast Thursday = 4
  cast Friday = 5
  cast Saturday = 6

public export
Cast DayOfWeek (Fin 7) where
  cast Sunday = 0
  cast Monday = 1
  cast Tuesday = 2
  cast Wednesday = 3
  cast Thursday = 4
  cast Friday = 5
  cast Saturday = 6


public export
Eq DayOfWeek where
  Monday == Monday = True
  Tuesday == Tuesday = True
  Wednesday == Wednesday = True
  Thursday == Thursday = True
  Friday == Friday = True
  Saturday == Saturday = True
  Sunday == Sunday = True
  _ == _ = False

public export
Ord DayOfWeek where
  compare x y = compare (cast {to=Fin 7} x) (cast {to=Fin 7} y)

public export
Show DayOfWeek where
  show Monday = "Monday"
  show Tuesday = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday = "Thursday"
  show Friday = "Friday"
  show Saturday = "Saturday"
  show Sunday = "Sunday"

-- ---------------------------------------------------------------------------

export
dayOfWeek : Day -> DayOfWeek
dayOfWeek d = cast $ restrict 6 $ d.modifiedJulianDay + 3


||| @dayOfWeekDiff a b = a - b@ in range 0 to 6.
||| The number of days from b to the next a.
export
dayOfWeekDiff : DayOfWeek -> DayOfWeek -> Int
dayOfWeekDiff a b = (the Int (cast a) - cast b) `mod` 7


||| The first day-of-week on or after some day
firstDayOfWeekOnAfter : DayOfWeek -> Day -> Day
firstDayOfWeekOnAfter dw d = addDays (cast $ dayOfWeekDiff dw $ dayOfWeek d) d




-- ---------------------------------------------------------------------------

public export data FirstWeekType
  = FirstWholeWeek   -- first week is the first whole week of the year
  | FirstMostWeek    -- first week is the first week with four days in the year

public export
Eq FirstWeekType where
  FirstWholeWeek == FirstWholeWeek = True
  FirstMostWeek == FirstMostWeek = True
  _ == _ = False



export firstDayOfWeekCalendar : FirstWeekType -> DayOfWeek -> Year -> Day
firstDayOfWeekCalendar wt dow year = let
  jan1st = fromOrdinalDate year 1
  in case wt of
          FirstWholeWeek => firstDayOfWeekOnAfter dow jan1st
          FirstMostWeek  => firstDayOfWeekOnAfter dow $ addDays (-3) jan1st


-- | Convert to the given kind of "week calendar".
-- Note that the year number matches the weeks, and so is not always the same as the Gregorian year number.
export
toWeekCalendar : FirstWeekType -- ^ how to reckon the first week of the year
              -> DayOfWeek     -- ^ the first day of each week
              -> Day
              -> (Year, WeekOfYear, DayOfWeek)
toWeekCalendar wt ws d = let
  dw      = dayOfWeek d
  (y0, _) = toOrdinalDate d
  j1p     = firstDayOfWeekCalendar wt ws $ y0 - 1
  j1      = firstDayOfWeekCalendar wt ws y0
  j1s     = firstDayOfWeekCalendar wt ws $ y0 + 1
  in if d < j1
        then (y0 - 1, (cast $ diffDays d j1p) `div` 7 + 1, dw)
        else if d < j1s then (y0, (cast $ diffDays d j1) `div` 7 + 1, dw)
        else (y0 + 1, (cast $ diffDays d j1s) `div` 7 + 1, dw)

                                                                                                                                    -- | Convert from the given kind of "week calendar".
-- Invalid week and day values will be clipped to the correct range.
export
fromWeekCalendar : FirstWeekType -- ^ how to reckon the first week of the year
                -> DayOfWeek -- ^ the first day of each week
                -> Year -> WeekOfYear -> DayOfWeek -> Day
fromWeekCalendar wt ws y wy dw = let
  d1 = firstDayOfWeekCalendar wt ws y
  wy' = max 1 $ min 53 wy
  d1s = firstDayOfWeekCalendar wt ws $ y + 1
  getday : WeekOfYear -> Day
  getday wy'' = addDays (cast $ ((wy'' - 1) * 7) + (dayOfWeekDiff dw ws)) d1
  day : Day
  day = getday wy'
  in if wy' == 53 && day >= d1s then getday 52 else day


||| Convert from the given kind of "week calendar".
||| Invalid week and day values will return Nothing.
export
fromWeekCalendarValid : FirstWeekType -- ^ how to reckon the first week of the year
                     -> DayOfWeek -- ^ the first day of each week
                     -> Year -> WeekOfYear -> DayOfWeek -> Maybe Day
fromWeekCalendarValid wt ws y wy dw = let
  d = fromWeekCalendar wt ws y wy dw
  in if toWeekCalendar wt ws d == (y,wy,dw) then Just d else Nothing

||| Convert to ISO 8601 Week Date format. First element of result is year, second week number (1-53), third day of week (1 for Monday to 7 for Sunday).
|||  Note that \"Week\" years are not quite the same as Gregorian years, as the first day of the year is always a Monday.
||| The first week of a year is the first week to contain at least four days in the corresponding Gregorian year.
export toWeekDate : Day -> (Year, WeekOfYear, Int)
toWeekDate d = let
  (y, wy, dw) = toWeekCalendar FirstMostWeek Monday d
  in (y, wy, cast dw)

||| Convert from ISO 8601 Week Date format. First argument is year, second week number (1-52 or 53), third day of week (1 for Monday to 7 for Sunday).
||| Invalid week and day values will be clipped to the correct range.
export fromWeekDate : Year -> WeekOfYear -> Int -> Day
fromWeekDate y wy dw =
   fromWeekCalendar FirstMostWeek Monday y wy (cast $ restrict 6 $ cast $ max 1 $ min 7 dw)

{-
||| Bidirectional abstract constructor for ISO 8601 Week Date format.
||| Invalid week values will be clipped to the correct range.
pattern YearWeekDay : Year -> WeekOfYear -> DayOfWeek -> Day
pattern YearWeekDay y wy dw <- (toWeekDate -> (y,wy,toEnum -> dw)) where
  YearWeekDay y wy dw = fromWeekDate y wy (fromEnum dw)
-}

||| Convert from ISO 8601 Week Date format. First argument is year, second week number (1-52 or 53), third day of week (1 for Monday to 7 for Sunday).
||| Invalid week and day values will return Nothing.
export fromWeekDateValid : Year -> WeekOfYear -> Int -> Maybe Day
fromWeekDateValid y wy dw =
  if dw < 1 || dw > 7
  then Nothing
  else fromWeekCalendarValid FirstMostWeek Monday y wy (cast $ restrict 6 $ cast dw)

{-
-- | Show in ISO 8601 Week Date format as yyyy-Www-d (e.g. \"2006-W46-3\").
showWeekDate : Day -> String
showWeekDate date = (show4 y) ++ "-W" ++ (show2 w) ++ "-" ++ (show d)
  where (y, w, d) = toWeekDate date
-}


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :

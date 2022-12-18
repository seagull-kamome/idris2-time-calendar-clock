module Data.Time.Calendar.OrdinalDate

import Data.Time.Calendar.Days
import public Data.Time.Calendar.Types

import Text.Format.Decimal

%default total


-- --------------------------------------------------------------------------

export isLeapYear : Year -> Bool
isLeapYear y = (y `mod` 4 == 0) && ((y `mod` 400 == 0) || (y `mod` 100 /= 0))


||| Convert to ISO 8601 Ordinal Date format.
export toOrdinalDate : Day -> (Year, DayOfYear)
toOrdinalDate (ModifiedJulianDay mjd) =
  let a        = mjd + 678575
      quadcent = a `div` 146097
      b        = a `mod` 146097
      cent     = min (b `div` 36524) 3
      c        = b - (cent * 36524)
      quad     = c `div` 1461
      d        = mod c 1461
      y        = min (d `div` 365) 3
   in (quadcent * 400 + cent * 100 + quad * 4 + y + 1,
       fromInteger (d - (y * 365) + 1))

showOrdinalDate : Day -> String
showOrdinalDate x with (toOrdinalDate x)
  showOrdinalDate x | (y, dy) = format' (zeroPad 4) y ++ format' (zeroPad 2) dy

||| Convert from ISO 8601 Ordinal Date format.
||| Invalid day numbers will be clipped to the correct range (1 to 365 or 366).
export fromOrdinalDate : Year -> DayOfYear -> Day
fromOrdinalDate year day =
  let y  = year - 1
      yl = if isLeapYear year then 366 else 365
      d' = cast $ if day < 1 then 1 else if day > yl then yl else day
    in ModifiedJulianDay $ d' + (365 * y) + (y `div` 4) - (y `div` 100) + (y `div` 400) - 678576


||| Convert from ISO 8601 Ordinal Date format.
||| Invalid day numbers return 'Nothing'
export fromOrdinalDateValid : Year -> DayOfYear -> Maybe Day
fromOrdinalDateValid year day =
  let y  = year - 1
      yl = if isLeapYear year then 366 else 365
    in if day < 0 || day > yl then Nothing
       else Just $ ModifiedJulianDay $ cast day + (365 * y) + (y `div` 4) - (y `div` 100) + (y `div` 400) - 678576

-- ||| Show in ISO 8601 Ordinal Date format (yyyy-ddd)
-- showOrdinalDate :: Day -> String
-- showOrdinalDate date = (show4 y) ++ "-" ++ (show3 d)
--   where
--         (y, d) = toOrdinalDate date

||| Get the number of the Monday-starting week in the year and the day of the week.
||| The first Monday is the first day of week 1, any earlier days in the year are week 0 (as @%W@ in 'Data.Time.Format.formatTime').
|||  Monday is 1, Sunday is 7 (as @%u@ in 'Data.Time.Format.formatTime').
export mondayStartWeek : Day -> (WeekOfYear, Int)
mondayStartWeek date =
  let yd = snd $ toOrdinalDate date
      d = date.modifiedJulianDay + 2
      k = d - cast yd
   in (fromInteger ((d `mod` 7) - (k `div` 7)), fromInteger (d `mod` 7) + 1)


||| Get the number of the Sunday-starting week in the year and the day of the week.
||| The first Sunday is the first day of week 1, any earlier days in the year are week 0 (as @%U@ in 'Data.Time.Format.formatTime').
||| Sunday is 0, Saturday is 6 (as @%w@ in 'Data.Time.Format.formatTime').
export sundayStartWeek : Day -> (WeekOfYear, Int)
sundayStartWeek date =
  let yd = snd $ toOrdinalDate date
      d  = date.modifiedJulianDay + 3
      k = d - cast yd
   in (fromInteger ((d `div` 7) - (k `div` 7)), fromInteger (mod d 7))


||| The inverse of 'mondayStartWeek'. Get a 'Day' given the year,
||| the number of the Monday-starting week, and the day of the week.
||| The first Monday is the first day of week 1, any earlier days in the year
||| are week 0 (as @%W@ in 'Data.Time.Format.formatTime').
export
fromMondayStartWeek : Year -- ^ Year.
                    -> WeekOfYear -- ^ Monday-starting week number (as @%W@ in 'Data.Time.Format.formatTime').
                    -> Int -- ^ Day of week.
                           -- Monday is 1, Sunday is 7 (as @%u@ in 'Data.Time.Format.formatTime').
                    -> Day
fromMondayStartWeek year w d =
  let firstDay = fromOrdinalDate year 1 -- first day of the year
      zbFirstMonday = (5 - firstDay.modifiedJulianDay) `mod` 7 -- 0-based year day of first monday of the year
      zbWeek = w - 1 -- 0-based week of year
      zbDay = d - 1 -- 0-based day of week
      zbYearDay = zbFirstMonday + 7 * cast zbWeek + cast zbDay -- 0-based day in year
   in addDays zbYearDay firstDay


export
fromMondayStartWeekValid : Year -- ^ Year.
                         -> WeekOfYear -- ^ Monday-starting week number (as @%W@ in 'Data.Time.Format.formatTime').
                         -> Int -- ^ Day of week.
                                -- Monday is 1, Sunday is 7 (as @%u@ in 'Data.Time.Format.formatTime').
                         -> Maybe Day
fromMondayStartWeekValid year w d =
 if d < 1 || d > 7 then Nothing
 else
   let firstDay = fromOrdinalDate year 1 -- first day of the year
       zbFirstMonday = (5 - firstDay.modifiedJulianDay) `mod` 7 -- 0-based week of year
       zbWeek = w - 1 -- 0-based week number
       zbDay = d - 1 -- 0-based day of week
       zbYearDay = zbFirstMonday + 7 * cast zbWeek + cast zbDay -- 0-based day in year
     in if zbYearDay < 0 || zbYearDay > (if isLeapYear year then 365 else 364)
           then Nothing
           else Just $ addDays zbYearDay firstDay



||| The inverse of 'sundayStartWeek'. Get a 'Day' given the year and
||| the number of the day of a Sunday-starting week.
||| The first Sunday is the first day of week 1, any earlier days in the
||| year are week 0 (as @%U@ in 'Data.Time.Format.formatTime').
export
fromSundayStartWeek : Year -- ^ Year.
                    -> WeekOfYear -- ^ Sunday-starting week number (as @%U@ in 'Data.Time.Format.formatTime').
                    -> Int -- ^ Day of week
                           -- Sunday is 0, Saturday is 6 (as @%w@ in 'Data.Time.Format.formatTime').
                    -> Day
fromSundayStartWeek year w d =
 let firstDay = fromOrdinalDate year 1 -- first day of the year
     zbFirstSunday = (4 - firstDay.modifiedJulianDay) `mod` 7 -- 0-based year day of first monday of the year
     zbWeek = w - 1 -- 0-based week of year
     zbDay = d -- 0-based day of week
     zbYearDay = zbFirstSunday + 7 * cast zbWeek + cast zbDay -- 0-based day in year
  in addDays zbYearDay firstDay


export
fromSundayStartWeekValid : Year -- ^ Year.
                         -> WeekOfYear -- ^ Sunday-starting week number (as @%U@ in 'Data.Time.Format.formatTime').
                         -> Int -- ^ Day of week.
                                -- Sunday is 0, Saturday is 6 (as @%w@ in 'Data.Time.Format.formatTime').
                         -> Maybe Day
fromSundayStartWeekValid year w d =
  if d < 0 || d > 6 then Nothing
  else
    let firstDay = fromOrdinalDate year 1 -- first day of the year
        zbFirstSunday = (4 - firstDay.modifiedJulianDay) `mod` 7 -- 0-based week of year
        zbWeek = w - 1 -- 0-based week number
        zbYearDay = zbFirstSunday + 7 * cast zbWeek + cast d -- 0-based day in year
     in if zbYearDay < 0 || zbYearDay > (if isLeapYear year then 365 else 364)
           then Nothing
           else Just $ addDays zbYearDay firstDay


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
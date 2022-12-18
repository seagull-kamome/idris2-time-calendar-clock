module Data.Time.Calendar.Gregorian

import public Data.Time.Calendar.Types
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.Days
import Data.Time.Calendar.CalendarDiffDays

import Text.Format.Decimal

%default total

-- --------------------------------------------------------------------------

||| Convert to proleptic Gregorian calendar.
export toGregorian : Day -> (Year, MonthOfYear, DayOfMonth)
toGregorian date =
  let (y, yd) = toOrdinalDate date
      (m, md) = dayOfYearToMonthAndDay (isLeapYear y) yd
      in (y, m, md)


||| Convert from proleptic Gregorian calendar.
||| Invalid values will be clipped to the correct range, month first, then day.
export fromGregorian : Year -> MonthOfYear -> DayOfMonth -> Day
fromGregorian year month day =
 fromOrdinalDate year (monthAndDayToDayOfYear (isLeapYear year) month day)



||| Convert from proleptic Gregorian calendar.
||| Invalid values will return Nothing
export fromGregorianValid : Year -> MonthOfYear -> DayOfMonth -> Maybe Day
fromGregorianValid year month day =
  case monthAndDayToDayOfYearValid (isLeapYear year) month day of
    Nothing => Nothing
    Just yd => fromOrdinalDateValid year yd



||| Show in ISO 8601 format (yyyy-mm-dd)
export showGregorian : Day -> String
showGregorian x with (toGregorian x)
  showGregorian x | (y, m, d) =
    format' (zeroPad 4) y ++ "-" ++ format' (zeroPad 2) m ++ "-" ++ format' (zeroPad 2) d

public export Show Day where show = showGregorian -- orphaned instance



||| The number of days in a given month according to the proleptic Gregorian calendar.
export gregorianMonthLength : Year -> MonthOfYear -> DayOfMonth
gregorianMonthLength year = monthLength $ isLeapYear year


export rolloverMonths : (Year, Integer) -> (Year, MonthOfYear)
rolloverMonths (y, m) = (y + ((m - 1) `div` 12), cast ((m - 1) `mod` 12) + 1)


export addGregorianMonths : Integer -> Day -> (Year, MonthOfYear, DayOfMonth)
addGregorianMonths n day =
  let (y, m, d) = toGregorian day
      (y', m') = rolloverMonths (y, cast m + n)
   in (y', m', d)



||| Add months, with days past the last day of the month clipped to the last day.
||| For instance, 2005-01-30 + 1 month = 2005-02-28.
export addGregorianMonthsClip : Integer -> Day -> Day
addGregorianMonthsClip n day =
  let (y, m, d) = addGregorianMonths n day
   in fromGregorian y m d



||| Add months, with days past the last day of the month rolling over to the next month.
||| For instance, 2005-01-30 + 1 month = 2005-03-02.
export addGregorianMonthsRollOver : Integer -> Day -> Day
addGregorianMonthsRollOver n day =
  let (y, m, d) = addGregorianMonths n day
   in addDays (cast d - 1) (fromGregorian y m 1)


||| Add years, matching month and day, with Feb 29th clipped to Feb 28th if necessary.
||| For instance, 2004-02-29 + 2 years = 2006-02-28.
export addGregorianYearsClip : Integer -> Day -> Day
addGregorianYearsClip n = addGregorianMonthsClip (n * 12)


||| Add years, matching month and day, with Feb 29th rolled over to Mar 1st if necessary.
||| For instance, 2004-02-29 + 2 years = 2006-03-01.
export addGregorianYearsRollOver : Integer -> Day -> Day
addGregorianYearsRollOver n = addGregorianMonthsRollOver (n * 12)


||| Add months (clipped to last day), then add days
export addGregorianDurationClip : CalendarDiffDays -> Day -> Day
addGregorianDurationClip x day =
  addDays x.day $ addGregorianMonthsClip x.month day


||| Add months (rolling over to next month), then add days
addGregorianDurationRollOver : CalendarDiffDays -> Day -> Day
addGregorianDurationRollOver x day =
  addDays x.day $ addGregorianMonthsRollOver x.month day


||| Calendrical difference, with as many whole months as possible
diffGregorianDurationClip : Day -> Day -> CalendarDiffDays
diffGregorianDurationClip day2 day1 =
  let (y1, m1, d1) = toGregorian day1
      (y2, m2, d2) = toGregorian day2
      ym1 = y1 * 12 + cast m1
      ym2 = y2 * 12 + cast m2
      ymdiff = ym2 - ym1
      ymAllowed = if day2 >= day1 then if d2 >= d1 then ymdiff else ymdiff - 1
                  else if d2 <= d1 then ymdiff
                  else ymdiff + 1
      dayAllowed = addGregorianDurationClip (MkCalendarDiffDays ymAllowed 0) day1
   in MkCalendarDiffDays ymAllowed $ diffDays day2 dayAllowed



||| Calendrical difference, with as many whole months as possible.
||| Same as 'diffGregorianDurationClip' for positive durations.
diffGregorianDurationRollOver : Day -> Day -> CalendarDiffDays
diffGregorianDurationRollOver day2 day1 =
  let (y1, m1, d1) = toGregorian day1
      (y2, m2, d2) = toGregorian day2
      ym1 = y1 * 12 + cast m1
      ym2 = y2 * 12 + cast m2
      ymdiff = ym2 - ym1
      ymAllowed = if day2 >= day1 then if d2 >= d1 then ymdiff else ymdiff - 1
                  else if d2 <= d1 then ymdiff
                  else ymdiff + 1
      dayAllowed = addGregorianDurationRollOver (MkCalendarDiffDays ymAllowed 0) day1
   in MkCalendarDiffDays ymAllowed $ diffDays day2 dayAllowed


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
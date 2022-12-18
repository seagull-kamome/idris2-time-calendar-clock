module Data.Time.Calendar.MonthDay

import Data.Vect
import Data.Fin
import public Data.Time.Calendar.Types

%default total

-- --------------------------------------------------------------------------


export monthLengths : Bool -> Vect 12 DayOfMonth
monthLengths isleap = [
  31, (if isleap then 29 else 28), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]


 -- | The length of a given month in the Gregorian or Julian calendars.
 -- First arg is leap year flag.
export monthLength : Bool -> MonthOfYear -> DayOfMonth
monthLength isLeap month = index (restrict 11 $ cast $ month - 1) $ monthLengths isLeap




||| Convert month and day in the Gregorian or Julian calendars to day of year.
||| First arg is leap year flag.
export monthAndDayToDayOfYear : Bool -> MonthOfYear -> DayOfMonth -> DayOfYear
monthAndDayToDayOfYear isLeap month day =
  let m' = if month < 1 then 1 else if month > 12 then 12 else month
      ml = monthLength isLeap m'
      d' = if day < 1 then 1 else if day > ml then ml else day
      k = if m' <= 2 then 0 else if isLeap then -1 else -2
   in ((367 * m' - 362) `div` 12) + cast k + d'


||| Convert month and day in the Gregorian or Julian calendars to day of year.
-- First arg is leap year flag.
export monthAndDayToDayOfYearValid : Bool -> MonthOfYear -> DayOfMonth -> Maybe DayOfYear
monthAndDayToDayOfYearValid isLeap month day =
  if month < 1 || month > 12
    then Nothing
    else
      let ml = monthLength isLeap month
          k = if month < 2 then 0 else if isLeap then -1 else -2
       in if day < 1 || day > ml
             then Nothing
             else Just $ ((367 * month - 362) `div` 12) + cast k + day


findMonthDay : Int -> Vect _ DayOfMonth -> Int -> (Int, Int)
findMonthDay m (n::ns) yd =
  if yd > n then findMonthDay (m + 1) ns (yd - n)
            else (m, yd)
findMonthDay m [] yd = (m, yd)


||| Convert day of year in the Gregorian or Julian calendars to month and day.
||| First arg is leap year flag.
export dayOfYearToMonthAndDay : Bool -> DayOfYear -> (MonthOfYear, DayOfMonth)
dayOfYearToMonthAndDay isLeap yd =
 let yl = the Int $ if isLeap then 366 else 365
  in findMonthDay 1
                  (monthLengths isLeap)
                  (if yd < 1 then 1 else if yd > yl then yl else yd)


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
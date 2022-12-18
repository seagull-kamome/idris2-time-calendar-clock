module Data.Time.Calendar.Julian

import Data.Time.Calendar.Days
import public Data.Time.Calendar.Types

%default total

-- ---------------------------------------------------------------------------

||| Is this year a leap year according to the proleptic Julian calendar?
isJulianLeapYear : Year -> Bool
isJulianLeapYear y = (y `mod` 4 == 0) && ((y `mod` 400 == 0) || (y `mod` 100 /= 0))



||| Convert to proleptic Julian year and day format.
toJulianYearAndDay : Day -> (Year, DayOfYear)
toJulianYearAndDay (ModifiedJulianDay mjd) =
  let a    = mjd + 678577
      quad = a `div` 1461
      d    = a `mod` 1461
      y    = min (d `div` 365) 3
    in (quad * 4 + y + 1, fromInteger $ d - (y * 365) + 1)


||| Convert from proleptic Julian year and day format.
||| Invalid day numbers will be clipped to the correct range (1 to 365 or 366).
fromJulianYearAndDay : Year -> DayOfYear -> Day
fromJulianYearAndDay year day =
  let y = year - 1
      yd = if isJulianLeapYear year then 366 else 365
      mjd = cast (if day < 1 then 1
            else if day > yd then yd
            else day) + (365 * y) + (y `div` 4) - 678578
    in ModifiedJulianDay mjd



||| Convert from proleptic Julian year and day format.
||| Invalid day numbers will return Nothing
fromJulianYearAndDayValid : Year -> DayOfYear -> Maybe Day
fromJulianYearAndDayValid year day =
  let y = year - 1
      yd = if isJulianLeapYear year then 366 else 365
    in if day < 1 then Nothing
       else if day > yd then Nothing
       else Just $ ModifiedJulianDay $ cast day + (365 * y) + (y `div` 4) - 678578


{-  -- FIXME:
||| Show in proleptic Julian year and day format (yyyy-ddd)
showJulianYearAndDay : Day -> String
showJulianYearAndDay date =
  let (y, d) = toJulianYearAndDay
   in (show4 y) ++ "-" ++ (show3 d)
-} 


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
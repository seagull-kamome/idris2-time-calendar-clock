module Data.Time.LocalTime

import Data.So
import Data.Rational

import public Data.Time.LocalTime.TimeZone
import public Data.Time.LocalTime.TimeOfDay
import public Data.Time.LocalTime.CalendarDiffTime
import public Data.Time.LocalTime.Internal.LocalTime

import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Clock.DiffTime
import Data.Time.Clock.UTCTime
import Data.Time.Clock


%default total

-- --------------------------------------------------------------------------


||| addLocalTime a b = a + b
export addLocalTime : NominalDiffTime -> LocalTime -> LocalTime
addLocalTime x = utcToLocalTime utc . addUTCTime x . localTimeToUTC utc


||| diffLocalTime a b = a - b
export diffLocalTime : LocalTime -> LocalTime -> NominalDiffTime
diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)

||| Get the local time of a UT1 time on a particular meridian (in degrees, positive is East).
export ut1ToLocalTime : Rational -> UniversalTime -> Maybe LocalTime
ut1ToLocalTime long date = let
  localTime = date.modJulianDate + long / 360
  in do
    localMJD  <- floor localTime
    dt <- dayFractionToTimeOfDay (localTime - cast localMJD)
    pure $ MkLocalTime (ModifiedJulianDay localMJD) dt

||| Get the UT1 time of a local time on a particular meridian (in degrees, positive is East).
export localTimeToUT1 : Rational -> LocalTime -> UniversalTime
localTimeToUT1 long lt =
    ModJulianDate (fromInteger lt.localDay.modifiedJulianDay
                  + (timeOfDayToDayFraction lt.localTimeOfDay) - (long / 360))

public export
Show UniversalTime where
    show t = show (ut1ToLocalTime 0 t)


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
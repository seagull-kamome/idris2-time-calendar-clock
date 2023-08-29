||| Zoned time - Ported from Haskell time package.
||| 
||| Copyright 2021-2023, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Data.Time.LocalTime.Internal.LocalTime

import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Data.Time.Clock.DiffTime
import Data.Time.Clock.Internal.UTCTime
import Data.Time.LocalTime.TimeZone
import Data.Time.LocalTime.TimeOfDay

import Generics.Derive
import Derive.Eq
import Derive.Ord

%default total
%language ElabReflection

-- --------------------------------------------------------------------------

||| A simple day and time aggregate, where the day is of the specified parameter,
||| and the time is a TimeOfDay.
||| Conversion of this (as local civil time) to UTC depends on the time zone.
||| Conversion of this (as local mean time) to UT1 depends on the longitude.
public export
record LocalTime where
  constructor MkLocalTime
  localDay : Day
  localTimeOfDay : TimeOfDay
%runElab derive "LocalTime" [Generic, Derive.Eq.Eq, Derive.Ord.Ord, DecEq]

public export
Show LocalTime where
  show x = show x.localDay ++ " " ++ show x.localTimeOfDay

-- --------------------------------------------------------------------------

||| Get the local time of a UTC time in a time zone.
export utcToLocalTime : TimeZone -> UTCTime -> LocalTime
utcToLocalTime tz (MkUTCTime day dt) = let
  (i, tod) = utcToLocalTimeOfDay tz (timeToTimeOfDay dt)
  in MkLocalTime (addDays i day) tod

||| Get the UTC time of a local time in a time zone.
export localTimeToUTC : TimeZone -> LocalTime -> UTCTime
localTimeToUTC tz (MkLocalTime day tod) = let
  (i, todUTC) = localToUTCTimeOfDay tz tod
  in MkUTCTime (addDays i day) (timeOfDayToTime todUTC)



-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :

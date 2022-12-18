module Data.Time.LocalTime.ZonedTime

import Data.Time.LocalTime.TimeZone
import Data.Time.LocalTime.Internal.LocalTime
import Data.Time.Clock.Internal.UTCTime
-- import Data.Time.Clock.System

import Generics.Derive

%default total
%language ElabReflection

-- ---------------------------------------------------------------------------

||| A local time together with a time zone.
|||
||| There is no 'Eq' instance for @ZonedTime@.
||| If you want to compare local times, use 'zonedTimeToLocalTime'.
||| If you want to compare absolute times, use 'zonedTimeToUTC'.
public export
record ZonedTime where
  constructor MkZonedTime
  localtime : LocalTime
  timezone : TimeZone
%runElab derive "ZonedTime" [Generic, Eq, Ord, DecEq]

public export
Show ZonedTime where
  show x = "\{show x.localtime}  \{show x.timezone}"


-- ---------------------------------------------------------------------------


export utcToZonedTime : TimeZone -> UTCTime -> ZonedTime
utcToZonedTime zone time = MkZonedTime (utcToLocalTime zone time) zone

export zonedTimeToUTC : ZonedTime -> UTCTime
zonedTimeToUTC x = localTimeToUTC x.timezone x.localtime

{-  FIXME: timezone
export getZonedTime : IO ZonedTime
getZonedTime = do
    t <- getCurrentTime
    zone <- getTimeZone t
    return (utcToZonedTime zone t)

export utcToLocalZonedTime : UTCTime -> IO ZonedTime
utcToLocalZonedTime t = do
    zone <- getTimeZone t
    return (utcToZonedTime zone t)
    -}


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :

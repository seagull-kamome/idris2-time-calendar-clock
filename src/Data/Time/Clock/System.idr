||| System clock time.
||| 
||| Copyright 2021, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Data.Time.Clock.System

import Data.Fixed

import Data.Time.Calendar.Types
import Data.Time.Calendar.Days
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.AbsoluteTime
import Data.Time.Clock.DiffTime
import Data.Time.LocalTime.TimeOfDay
import Data.Time.LocalTime.TimeZone

import System.Clock as CLK
import System.Info


%default total

-- ---------------------------------------------------------------------------

public export
data SystemTime : Type where [external]


%foreign "scheme,chez:current-time"
         "javascript:lambda:() => new Date()"
         "RefC:clockTimeUtc"
prim__systime_get : PrimIO SystemTime


export getSystemTime : HasIO io => io SystemTime
getSystemTime = primIO $ prim__systime_get


%foreign "scheme,chez:time-second"
         "javascript:lambda:(x) => (x.getTime() / 1000)"
         "RefC:clockSecond"
systemSeconds : SystemTime -> Int


%foreign "scheme,chez:time-nanosecond"
         "javascript:lambda:(x) => ((x.getTime() % 1000)* 1000000)"
         "RefC:clockNanosecond"
systemNanoseconds : SystemTime -> Int


-- On RefC and rc2 a 'SystemTime' is a boxed Bits64 count of nanoseconds since
-- the epoch (what 'clockTimeUtc' gives, and 'clockSecond', 'clockNanosecond'
-- read), so one made from a count is the runtime's own 'mkBits64'. A time before
-- the epoch cannot be made there.
%foreign "scheme,chez:(lambda (ns) (make-time 'time-utc (mod ns 1000000000) (div ns 1000000000)))"
         "javascript:lambda:(ns) => new Date(Math.floor(ns / 1000000))"
         "RefC:idris2_mkBits64"
         "RC2:idris2rc2_mkBits64"
toSystemTimeNs : Int -> SystemTime


toSystemTime : Int -> Int -> SystemTime
toSystemTime s ns = toSystemTimeNs (s * 1000000000 + ns)



public export
Show SystemTime where
  show x = "#\{show (systemSeconds x)}s+\{show (systemNanoseconds x)}ns"


||| Map leap-second values to the start of the following second.
||| The resulting 'systemNanoseconds' will always be in the range 0 to 1E9-1.
export truncateSystemTimeLeapSecond : SystemTime -> SystemTime
truncateSystemTimeLeapSecond t =
  if systemNanoseconds t >= 1000000000
     then toSystemTime (systemSeconds t + 1) 0
     else t


||| The day of the epoch of 'SystemTime', 1970-01-01
export systemEpochDay : Day
systemEpochDay = ModifiedJulianDay 40587

export systemEpochAbsolute : AbsoluteTime
systemEpochAbsolute = taiNominalDayStart systemEpochDay





||| Convert 'SystemTime' to 'UTCTime', matching zero 'SystemTime' to midnight of 'systemEpochDay' UTC.
export systemToUTCTime : SystemTime -> UTCTime
systemToUTCTime t = let
  days = cast $ systemSeconds t `div` 86400
  day = addDays days systemEpochDay
  --
  timeseconds = systemSeconds t `mod` 86400
  timeNanoseconds = cast timeseconds * 1000000000 + cast (systemNanoseconds t)
  timePicoseconds = cast timeNanoseconds * 1000
  in MkUTCTime day $ picosecondsToDiffTime timePicoseconds


export getCurrentTime : HasIO io => io UTCTime
getCurrentTime = [| systemToUTCTime getSystemTime |]


||| Convert 'UTCTime' to 'SystemTime', matching zero 'SystemTime' to midnight of 'systemEpochDay' UTC.
export utcToSystemTime : UTCTime -> SystemTime
utcToSystemTime (MkUTCTime day time) = let
    days = diffDays day systemEpochDay
    timeNanoseconds = diffTimeToPicoseconds time `div` 1000
    (timeSeconds, nanoseconds) =
        if timeNanoseconds >= 86400000000000
            then (86399, timeNanoseconds - 86399000000000)
            else (timeNanoseconds `div` 1000000000, timeNanoseconds `mod` 1000000000)
    seconds = days * 86400 + timeSeconds
    in toSystemTime (cast seconds) $ cast nanoseconds


||| Convert 'SystemTime' to 'AbsoluteTime', matching zero 'SystemTime' to midnight of 'systemEpochDay' TAI.
export systemToTAITime : SystemTime -> AbsoluteTime
systemToTAITime t = let
    s = cast $ systemSeconds t
    ns = systemNanoseconds t
    diff = secondsToDiffTime (fromInteger s) + picosecondsToDiffTime (cast ns * 1000)
    in addAbsoluteTime diff systemEpochAbsolute





-- ---------------------------------------------------------------------------

public export data SystemLocalTime : Type where [external]


-- On RefC and rc2 a 'SystemLocalTime' is the same boxed count of nanoseconds as a
-- 'SystemTime'; on rc2 the fields are read with 'localtime_r'. RefC has nothing
-- in its runtime that reads them.
%foreign "scheme,chez:current-date"
         "javascript:lambda:() => new Date()"
         "RefC:clockTimeUtc"
export prim__getSystemLocalTime : PrimIO SystemLocalTime

export getSystemLocalTime : HasIO io => io SystemLocalTime
getSystemLocalTime = primIO prim__getSystemLocalTime


%foreign "scheme,chez:date-year"
         "javascript:lambda:(x) => x.getFullYear()"
         "CExpr:({ time_t t_ = (time_t)(idris2rc2_to_u64($1) / 1000000000ULL); struct tm tm_; localtime_r(&t_, &tm_); (int64_t)(tm_.tm_year + 1900); }),libc,time.h"
prim__systemLocalTimeYear : SystemLocalTime -> Int


export systemLocalTimeYear : SystemLocalTime -> Integer
systemLocalTimeYear = cast . prim__systemLocalTimeYear



%foreign "scheme,chez:date-month"
         "javascript:lambda:(x) => x.getMonth() + 1"
         "CExpr:({ time_t t_ = (time_t)(idris2rc2_to_u64($1) / 1000000000ULL); struct tm tm_; localtime_r(&t_, &tm_); (int64_t)(tm_.tm_mon + 1); }),libc,time.h"
export systemLocalTimeMonth : SystemLocalTime -> Int


%foreign "scheme,chez:date-day"
         "javascript:lambda:(x) => x.getDate()"
         "CExpr:({ time_t t_ = (time_t)(idris2rc2_to_u64($1) / 1000000000ULL); struct tm tm_; localtime_r(&t_, &tm_); (int64_t)(tm_.tm_mday); }),libc,time.h"
export systemLocalTimeDay : SystemLocalTime -> Int


%foreign "scheme,chez:date-hour"
         "javascript:lambda:(x) => x.getHours()"
         "CExpr:({ time_t t_ = (time_t)(idris2rc2_to_u64($1) / 1000000000ULL); struct tm tm_; localtime_r(&t_, &tm_); (int64_t)(tm_.tm_hour); }),libc,time.h"
export systemLocalTimeHour : SystemLocalTime -> Int


%foreign "scheme,chez:date-minute"
         "javascript:lambda:(x) => x.getMinutes()"
         "CExpr:({ time_t t_ = (time_t)(idris2rc2_to_u64($1) / 1000000000ULL); struct tm tm_; localtime_r(&t_, &tm_); (int64_t)(tm_.tm_min); }),libc,time.h"
export systemLocalTimeMinute : SystemLocalTime -> Int

%foreign "scheme,chez:date-second"
         "javascript:lambda:(x) => x.getSeconds()"
         "CExpr:({ time_t t_ = (time_t)(idris2rc2_to_u64($1) / 1000000000ULL); struct tm tm_; localtime_r(&t_, &tm_); (int64_t)(tm_.tm_sec); }),libc,time.h"
export systemLocalTimeSecond : SystemLocalTime -> Int

%foreign "scheme,chez:date-nanosecond"
         "javascript:lambda:(x) => (x.getMilliseconds() * 1000000)"
         "CExpr:(int64_t)(idris2rc2_to_u64($1) % 1000000000ULL)"
export systemLocalTimeNanosecond : SystemLocalTime -> Int


%foreign "scheme,chez:date-zone-offset"
         "javascript:lambda:(x) => (x.getTimezoneOffset() * 60)"
         "CExpr:({ time_t t_ = (time_t)(idris2rc2_to_u64($1) / 1000000000ULL); struct tm tm_; localtime_r(&t_, &tm_); (int64_t)(-tm_.tm_gmtoff); }),libc,time.h"
export systemLocalTimeOffset : SystemLocalTime -> Int


-- The name of the zone and whether it is summer time are not known on any
-- backend: they were constants (Chez, JavaScript) and are plain definitions now,
-- so that no backend needs a foreign function for them.
export systemLocalTimeZoneName : SystemLocalTime -> String
systemLocalTimeZoneName _ = ""


export systemLocalTimeIsSummerTime : SystemLocalTime -> Bool
systemLocalTimeIsSummerTime _ = False



export systemLocalDate : SystemLocalTime -> (Year, MonthOfYear, DayOfMonth)
systemLocalDate t = let
  y = systemLocalTimeYear t
  m = systemLocalTimeMonth t
  d = systemLocalTimeDay t
  in (cast y, m, d)


export systemLocalTimeOfDay : SystemLocalTime -> TimeOfDay
systemLocalTimeOfDay t = let
  h = systemLocalTimeHour t
  m = systemLocalTimeMinute t
  s = systemLocalTimeSecond t
  ns = systemLocalTimeNanosecond t
  in MkTimeOfDay h m (cast s + MkFixed (cast $ ns * 1000))


export systemLocalTimeZone : SystemLocalTime -> TimeZone
systemLocalTimeZone t = let
  m = negate $ systemLocalTimeOffset t `div` 60
  in MkTimeZone m (systemLocalTimeIsSummerTime t) (systemLocalTimeZoneName t)


export getCurrentTimeZone : IO TimeZone
getCurrentTimeZone = getSystemLocalTime >>= pure . systemLocalTimeZone


public export
Show SystemLocalTime where
  show x = let
    (y, m, d) = systemLocalDate x
    td = systemLocalTimeOfDay x
    tz = systemLocalTimeZone x
    in "\{show y}-\{show m}-\{show d} \{show td} \{show tz}"


-- vim: tw=80 sw=2 expandtab :

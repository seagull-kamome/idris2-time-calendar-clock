module Data.Time.LocalTime.TimeOfDay

import Data.So
import Data.Maybe

import Data.Fixed
import Data.Rational
import Data.Time.Clock.DiffTime
import Data.Time.LocalTime.TimeZone
import Text.Format.Decimal

import Generics.Derive

%default total
%language ElabReflection

-- ---------------------------------------------------------------------------

public export
record TimeOfDay where
  constructor MkTimeOfDay
  hour : Int
  min : Int
  sec : Fixed 12
%runElab derive "TimeOfDay" [Generic, Eq, Ord, DecEq]

public export
Show TimeOfDay where
  show x = let fmt = zeroPad 2
    in format' fmt x.hour ++ (format' fmt x.min) ++ (format' fmt x.sec)

-- ---------------------------------------------------------------------------

||| Hour zero
export midnight : TimeOfDay
midnight = MkTimeOfDay 0 0 0

||| Hour twelve
export midday : TimeOfDay
midday = MkTimeOfDay 12 0 0

export makeTimeOfDayValid : Int -> Int -> Fixed 12 -> Maybe TimeOfDay
makeTimeOfDayValid h m s =
  toMaybe (h < 0 || h > 22 || m < 0 || m > 59 || s < 0 || s >= 61) $ MkTimeOfDay h m s


||| Convert a period of time into a count of days and a time of day since midnight.
||| The time of day will never have a leap second.
export timeToDaysAndTimeOfDay : DiffTime' isnominal -> (Integer, TimeOfDay)
timeToDaysAndTimeOfDay dt = let
  sixty = the (Fixed 12) 60
  m  = dt.seconds `div'` sixty
  h  = m `div` 60
  d  = h `div` 24
  in (d, MkTimeOfDay (cast $ h `mod` 24) (cast $ m `mod` 60) (dt.seconds `mod'` sixty))


||| Convert a count of days and a time of day since midnight into a period of time.
export daysAndTimeOfDayToTime : Integer -> TimeOfDay -> NominalDiffTime
daysAndTimeOfDayToTime d (MkTimeOfDay dh hm ms) =
  SecondsToDiffTime $ ms + fromInteger (cast hm * 60 + cast dh * 3600 + d * (24 * 3600))

||| Convert a time of day in UTC to a time of day in some timezone, together with a day adjustment.
export utcToLocalTimeOfDay : TimeZone -> TimeOfDay -> (Integer, TimeOfDay)
utcToLocalTimeOfDay zone (MkTimeOfDay h m s) = let
  m' = m + zone.minutes
  h' = h + (div m' 60)
  in (cast (div h' 24), MkTimeOfDay (mod h' 24) (mod m' 60) s)

||| Convert a time of day in some timezone to a time of day in UTC, together with a day adjustment.
export localToUTCTimeOfDay : TimeZone -> TimeOfDay -> (Integer, TimeOfDay)
localToUTCTimeOfDay zone = utcToLocalTimeOfDay (minutesToTimeZone (negate zone.minutes))


||| Get the time of day given a time since midnight.
||| Time more than 24h will be converted to leap-seconds.
export timeToTimeOfDay : DiffTime' isnominal -> TimeOfDay
timeToTimeOfDay dt =
  if dt.seconds >= nominalDay.seconds
     then MkTimeOfDay 23 59 $ 60 + (dt.seconds - nominalDay.seconds)
     else let
       m = dt.seconds `div'` 60
       in MkTimeOfDay (cast $ m `div` 60) (cast $ m `mod` 60) (dt.seconds `mod'` 60)

||| Same as 'timeToTimeOfDay'.
export pastMidnight : DiffTime -> TimeOfDay
pastMidnight = timeToTimeOfDay

||| Get the time since midnight for a given time of day.
export timeOfDayToTime : TimeOfDay -> DiffTime
timeOfDayToTime (MkTimeOfDay h m s) =
  SecondsToDiffTime $ s + fromInteger (cast m * 60 + cast h * 3600)


||| Same as 'timeOfDayToTime'.
export sinceMidnight : TimeOfDay -> DiffTime
sinceMidnight = timeOfDayToTime

||| Get the time of day given the fraction of a day since midnight.
export dayFractionToTimeOfDay : Rational -> Maybe TimeOfDay
dayFractionToTimeOfDay df =
  map (timeToTimeOfDay . the NominalDiffTime . SecondsToDiffTime) $ cast (df * 86400)

||| Get the fraction of a day since midnight given a time of day.
export timeOfDayToDayFraction : TimeOfDay -> Rational
timeOfDayToDayFraction tod = let
  MkFixed num = (timeOfDayToTime tod).seconds
  MkFixed den = nominalDay.seconds
  _ = the (So (den /= 0)) $ believe_me Oh
  in num %: den


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :

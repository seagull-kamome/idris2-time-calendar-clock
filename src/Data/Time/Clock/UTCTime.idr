module Data.Time.Clock.UTCTime

import public Data.Time.Clock.Internal.UTCTime

import Data.Fixed
import Data.Time.Clock.DiffTime
import Data.Time.Calendar.Days
import Data.Time.LocalTime.Internal.LocalTime
import Data.Time.LocalTime.TimeZone
import Data.Time.LocalTime.ZonedTime


%default total
-- ---------------------------------------------------------------------------

-- orphan instance
public export Show UTCTime where show t = show $ utcToZonedTime utc t



export addUTCTime : NominalDiffTime -> UTCTime -> UTCTime
addUTCTime a b =
  let s = a.seconds + b.daytime.seconds
      d = s `div'` nominalDay.seconds
      t = s `mod'` nominalDay.seconds
      in MkUTCTime (addDays d b.day) (secondsToDiffTime t)


export diffUTCTime : UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime a b =
  let d = diffDays a.day b.day
      t = a.daytime.seconds - b.daytime.seconds
      in SecondsToDiffTime $ scale d nominalDay.seconds + t



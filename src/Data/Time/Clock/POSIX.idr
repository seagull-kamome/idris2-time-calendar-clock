module Data.Time.Clock.POSIX

import Data.Fixed

import Data.Time.Clock.DiffTime
import Data.Time.Clock.UTCTime
import Data.Time.Calendar

%default total

-- --------------------------------------------------------------------------

public export POSIXTime : Type
POSIXTime = NominalDiffTime

public export posixDayLength : POSIXTime
posixDayLength = nominalDay

public export posixEpochDay : Day
posixEpochDay = ModifiedJulianDay 40587

public export
Cast POSIXTime UTCTime where
  cast i =
    let d = i.seconds `div'` posixDayLength.seconds
        t = i.seconds `mod'` posixDayLength.seconds
        in MkUTCTime (addDays d posixEpochDay) (secondsToDiffTime t)

public export
Cast UTCTime POSIXTime where
  cast (MkUTCTime d t) =
    let MkFixed l = posixDayLength.seconds
        ds = posixDayLength.seconds
     in secondsToDiffTime $ (scale (diffDays d posixEpochDay) ds) + (min ds t.seconds)




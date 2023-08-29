||| Absolute time
||| 
||| Copyright 2021-2023, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Data.Time.Clock.AbsoluteTime

import Data.Fixed
import Data.Time.Calendar.Days
import Data.Time.Clock.DiffTime

import Generics.Derive
import Derive.Eq
import Derive.Ord
import Derive.Show

%default total
%language ElabReflection

-- --------------------------------------------------------------------------

export data AbsoluteTime = MkAbsoluteTime DiffTime
%runElab derive "AbsoluteTime" [Generic, Meta, Derive.Eq.Eq, Derive.Ord.Ord, Derive.Show.Show, DecEq]


||| The epoch of TAI, which is 1858-11-17 00:00:00 TAI.
export taiEpoch : AbsoluteTime
taiEpoch = MkAbsoluteTime 0


export taiNominalDayStart : Day -> AbsoluteTime
taiNominalDayStart day =
  MkAbsoluteTime $ SecondsToDiffTime $ cast $ day.modifiedJulianDay * 86400

||| addAbsoluteTime a b = a + b
export addAbsoluteTime : DiffTime -> AbsoluteTime -> AbsoluteTime
addAbsoluteTime t (MkAbsoluteTime a) = MkAbsoluteTime (a + t)

||| diffAbsoluteTime a b = a - b
export diffAbsoluteTime : AbsoluteTime -> AbsoluteTime -> DiffTime
diffAbsoluteTime (MkAbsoluteTime a) (MkAbsoluteTime b) = a - b


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :

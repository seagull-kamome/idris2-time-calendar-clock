||| Universal clock - Ported from Haskell time package.
||| 
||| Copyright 2021-2023, HATTORI, Hiroki
||| This file is released under the MIT license, see LICENSE for more detail.
||| 
module Data.Time.Clock

import Data.Fixed
import Data.Rational

import public Data.Time.Clock.DiffTime
import public Data.Time.Clock.UTCTime
import Data.Time.Calendar

import Generics.Derive
import Derive.Eq
import Derive.Show

%default total
%language ElabReflection

-- ---------------------------------------------------------------------------

public export
record UniversalTime where
  constructor ModJulianDate
  modJulianDate : Rational
%runElab derive "UniversalTime" [Generic, Meta, Derive.Eq.Eq, Derive.Show.Show]

public export
Ord UniversalTime where
  compare x y = compareAsReal x.modJulianDate y.modJulianDate

-- ---------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :

||| Day
|||
||| Copyright 2021. HIROKI, Hattori
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Time.Calendar.Days

import Generics.Derive
import Derive.Eq
import Derive.Ord
import Derive.Show

%default total
%language ElabReflection

-- ---------------------------------------------------------------------------

public export
record Day where
  constructor ModifiedJulianDay
  modifiedJulianDay : Integer
%runElab derive "Day" [Generic, Meta, Derive.Eq.Eq, Derive.Ord.Ord, Derive.Show.Show, DecEq]


export addDays : Integer -> Day -> Day
addDays n x = { modifiedJulianDay $= (+n) } x

export diffDays : Day -> Day -> Integer
diffDays x y = x.modifiedJulianDay - y.modifiedJulianDay


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :

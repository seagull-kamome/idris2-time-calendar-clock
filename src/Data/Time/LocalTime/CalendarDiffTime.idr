module Data.Time.LocalTime.CalendarDiffTime

import Data.Fixed
import Data.Time.Clock.DiffTime
import Data.Time.Calendar.CalendarDiffDays

import Generics.Derive

%default total
%language ElabReflection

-- --------------------------------------------------------------------------

public export
record CalendarDiffTime where
  constructor MkCalendarDiffTime
  month : Integer
  time : NominalDiffTime
%runElab derive "CalendarDiffTime" [Generic, Eq, DecEq]


public export
Semigroup CalendarDiffTime where
  x <+> y = MkCalendarDiffTime (x.month + y.month) (x.time + y.time)
public export
Monoid CalendarDiffTime where
  neutral = MkCalendarDiffTime 0 0

public export
Show CalendarDiffTime where
  show x = "P\{show x.month}MT\{show x.time}"

export calendarTimeDays : CalendarDiffDays -> CalendarDiffTime
calendarTimeDays x = MkCalendarDiffTime x.month $ daysToDiffTime x.day

export calendarTimeTime : NominalDiffTime -> CalendarDiffTime
calendarTimeTime dt = MkCalendarDiffTime 0 dt

||| Scale by a factor. Note that @scaleCalendarDiffTime (-1)@ will not perfectly invert a duration, due to variable month lengths.
export scaleCalendarDiffTime : Integer -> CalendarDiffTime -> CalendarDiffTime
scaleCalendarDiffTime k x = MkCalendarDiffTime (k * x.month) (scale k x.time)

-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :

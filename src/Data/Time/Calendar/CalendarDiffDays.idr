||| Date difference
|||
||| Copyright 2021. HIROKI, Hattori
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Time.Calendar.CalendarDiffDays

import Generics.Derive

%default total
%language ElabReflection

-- ---------------------------------------------------------------------------

public export
record CalendarDiffDays where
  constructor MkCalendarDiffDays
  month : Integer
  day : Integer
%runElab derive "CalendarDiffDays" [Generic, Meta, Eq, DecEq, Ord]


public export
Show CalendarDiffDays where
  show x = "P\{show  x.month}M\{show x.day}D"

public export
Semigroup CalendarDiffDays where
  x <+> y = MkCalendarDiffDays (x.month + y.month) (x.day + y.day)

public export
Monoid CalendarDiffDays where
  neutral = MkCalendarDiffDays 0 0


-- ---------------------------------------------------------------------------

export calendarDay : CalendarDiffDays
calendarDay = MkCalendarDiffDays 0 1

export calendarWeek : CalendarDiffDays
calendarWeek = MkCalendarDiffDays 0 7

export calendarMonth : CalendarDiffDays
calendarMonth = MkCalendarDiffDays 1 0

export calendarYear : CalendarDiffDays
calendarYear = MkCalendarDiffDays 12 0

||| Scale by a factor. Note that @scaleCalendarDiffDays (-1)@ will not perfectly invert a duration, due to variable month lengths.
scaleCalendarDiffDays : Integer -> CalendarDiffDays -> CalendarDiffDays
scaleCalendarDiffDays k (MkCalendarDiffDays m d) =
  MkCalendarDiffDays (k * m) (k * d)

-- vim: tw=80 sw=2 expandtab :

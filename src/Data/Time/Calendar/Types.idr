||| Basic types.
|||
||| Copyright 2021-2023. HIROKI, Hattori
||| This file is released under the MIT license, see LICENSE for more detail.
|||
module Data.Time.Calendar.Types

%default total

-- --------------------------------------------------------------------------

public export Year : Type
Year = Integer

public export MonthOfYear : Type
MonthOfYear = Int

public export DayOfMonth : Type
DayOfMonth = Int

public export DayOfYear : Type
DayOfYear = Int

public export WeekOfYear : Type
WeekOfYear = Int


-- --------------------------------------------------------------------------
-- vim : tw=80 sw=2 expandtab :
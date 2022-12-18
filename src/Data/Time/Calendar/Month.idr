module Data.Time.Calendar.Month

import public Data.Time.Calendar.Types
import Data.Time.Calendar.Days
import Data.Time.Calendar.Gregorian
import Text.Format.Decimal

import Generics.Derive

%default total
%language ElabReflection

-- ---------------------------------------------------------------------------

||| An absolute count of common calendar months.
||| Number is equal to @(year * 12) + (monthOfYear - 1)@.
export data Month = MkMonth Integer

public export Cast Month Integer where cast (MkMonth x) = x
public export Cast Integer Month where cast = MkMonth

%runElab derive "Month" [Generic, Meta, Eq, Ord]

-- ---------------------------------------------------------------------------

export addMonths : Integer -> Month -> Month
addMonths n (MkMonth x) = MkMonth $ x + n

export diffMonths : Month -> Month -> Integer
diffMonths (MkMonth x) (MkMonth y) = x - y

export toYearMonth : Month -> (Year, MonthOfYear)
toYearMonth (MkMonth x) =
  let y = x `div` 12
      m = cast $ x `mod` 12
    in (y, m + 1)

export fromYearMonth : Year -> MonthOfYear -> Month
fromYearMonth y m =
 let m' = the Int $ if m < 1 then 1 else if m > 12 then 12 else m
  in MkMonth $ y * 12 + cast (m' - 1)

export fromYearMonthValid : Year -> MonthOfYear -> Maybe Month
fromYearMonthValid y m =
  if m < 1 || m > 12 then Nothing
  else Just $ MkMonth $ y * 12 + cast (m - 1)


export toMonthDay : Day -> (Month, DayOfMonth)
toMonthDay d = let
  (y, m, d) = toGregorian d
  in (MkMonth (y * 12 + cast (max 0 $ min 11 m)), d)


export fromMonth : Month -> DayOfMonth -> Day
fromMonth m dm = let
  (y, my) = toYearMonth m
  in fromGregorian y my dm


export fromMonthDayValid : Month -> DayOfMonth -> Maybe Day
fromMonthDayValid m dm = let 
  (y, my) = toYearMonth m
  in fromGregorianValid y my dm


public export
Show Month where
  show m = let
    (y, my) = toYearMonth m
    in format' {width:=Just 4, pad:=Just '0'} y
       ++ format' {width:=Just 2, pad:=Just '0'} my


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :

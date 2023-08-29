||| UTC Time
|||
||| Copyrigh 2023. Hattori,Hiroki
||| See LICENSE for more detail.
module Data.Time.Clock.Internal.UTCTime

import Data.Time.Clock.DiffTime
import Data.Time.Calendar.Days

import Generics.Derive
import Derive.Eq
import Derive.Ord

%default total
%language ElabReflection

-- --------------------------------------------------------------------------

public export
record UTCTime where
  constructor MkUTCTime
  day : Day
  daytime : DiffTime
%runElab derive "UTCTime" [Generic, Derive.Eq.Eq, Derive.Ord.Ord]

-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
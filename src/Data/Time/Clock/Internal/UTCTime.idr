module Data.Time.Clock.Internal.UTCTime

import Data.Time.Clock.DiffTime
import Data.Time.Calendar.Days

import Generics.Derive

%default total
%language ElabReflection

-- --------------------------------------------------------------------------

public export
record UTCTime where
  constructor MkUTCTime
  day : Day
  daytime : DiffTime
%runElab derive "UTCTime" [Generic, Eq, Ord]

-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :
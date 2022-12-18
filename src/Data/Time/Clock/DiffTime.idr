module Data.Time.Clock.DiffTime

import Data.So
import Data.Fixed

import Generics.Derive

%default total
%language ElabReflection

-- --------------------------------------------------------------------------

public export
record DiffTime' (nominal:Bool) where
  constructor SecondsToDiffTime
  seconds : Fixed 12
%runElab derive "DiffTime'" [Generic, Eq, Ord, DecEq]

public export DiffTime : Type
DiffTime = DiffTime' False

public export NominalDiffTime : Type
NominalDiffTime = DiffTime' True


--public export Eq (DiffTime' b) where x == y = x.seconds == y.seconds
--public export Ord (DiffTime' b) where compare x y = compare x.seconds y.seconds
public export
Num (DiffTime' b) where
  x + y = SecondsToDiffTime $ x.seconds + y.seconds
  x * y = SecondsToDiffTime $ x.seconds * y.seconds
  fromInteger x = SecondsToDiffTime $ fromInteger x
public export
Neg (DiffTime' b) where
  negate x = { seconds $= negate } x
  x - y = SecondsToDiffTime $ x.seconds - y.seconds

public export Show (DiffTime' b) where show x = show x.seconds ++ "s"


-- --------------------------------------------------------------------------

export %inline picosecondsToDiffTime : {nominal:Bool} -> Integer -> DiffTime' nominal
picosecondsToDiffTime x = SecondsToDiffTime $ MkFixed x

export %inline secondsToDiffTime : {nominal:Bool} -> Fixed 12 -> DiffTime' nominal
secondsToDiffTime x = SecondsToDiffTime x

export %inline diffTimeToPicoseconds : {nominal:Bool} -> DiffTime' nominal -> Integer
diffTimeToPicoseconds x = let MkFixed x' = x.seconds in x'

export %inline nominalDay : NominalDiffTime
nominalDay = 86400


export %inline scale : Integer -> DiffTime' b -> DiffTime' b
scale n dt = SecondsToDiffTime $ scale n dt.seconds

export %inline daysToDiffTime : Integer -> NominalDiffTime
daysToDiffTime n = scale n nominalDay


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :

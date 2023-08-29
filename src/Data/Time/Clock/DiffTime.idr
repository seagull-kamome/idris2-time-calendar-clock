||| DiffTime
|||
||| Copyrigh 2023. Hattori,Hiroki
||| See LICENSE for more detail.
module Data.Time.Clock.DiffTime

import Data.So
import Data.Fixed
import Control.Algebra

import Generics.Derive
import Derive.Eq
import Derive.Ord

%default total
%language ElabReflection

-- --------------------------------------------------------------------------
public export
record DiffTime' (nominal:Bool) where
  constructor SecondsToDiffTime
  seconds : Fixed 12
%runElab derive "DiffTime'" [Generic, Derive.Eq.Eq, Derive.Ord.Ord, DecEq]

public export DiffTime : Type
DiffTime = DiffTime' False

public export NominalDiffTime : Type
NominalDiffTime = DiffTime' True


-- --------------------------------------------------------------------------

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

-- --------------------------------------------------------------------------
public export
Semigroup (DiffTime' b) where x <+> y = x + y

public export
Monoid (DiffTime' b) where neutral = SecondsToDiffTime 0


-- --------------------------------------------------------------------------
public export Show (DiffTime' b) where show x = "\{show x.seconds}ss"


-- --------------------------------------------------------------------------

export %inline picosecondsToDiffTime : {0 nominal:Bool} -> Integer -> DiffTime' nominal
picosecondsToDiffTime x = SecondsToDiffTime $ MkFixed x

export %inline secondsToDiffTime : {0 nominal:Bool} -> Fixed 12 -> DiffTime' nominal
secondsToDiffTime x = SecondsToDiffTime x

export %inline diffTimeToPicoseconds : {0 nominal:Bool} -> DiffTime' nominal -> Integer
diffTimeToPicoseconds x = let MkFixed x' = x.seconds in x'

export %inline nominalDay : NominalDiffTime
nominalDay = secondsToDiffTime 86400


export %inline scale : Integer -> DiffTime' b -> DiffTime' b
scale n dt = SecondsToDiffTime $ scale n dt.seconds

export %inline daysToDiffTime : Integer -> NominalDiffTime
daysToDiffTime n = scale n nominalDay


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :

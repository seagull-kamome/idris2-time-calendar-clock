module Data.Time.LocalTime.TimeZone

import Text.Format.Decimal

import Generics.Derive

%default total
%language ElabReflection

-- ---------------------------------------------------------------------------

public export
record TimeZone where
  constructor MkTimeZone
  ||| The minutes offset from UTC. positive means local time will be later in the day than UTC.
  minutes : Int

  ||| Is this timezone just persisting for the summer?
  summeronly : Bool
  name : String
%runElab derive "TimeZone" [Generic, Eq, Ord, DecEq]



public export
record TimeZoneFormat where
  constructor MkTimeZoneFormat
  width : Maybe Nat
  sep : Maybe Char
  pad : Maybe Char

export defaultTimeZoneFormat : TimeZoneFormat
defaultTimeZoneFormat = MkTimeZoneFormat Nothing Nothing Nothing


export timeZoneOffsetString' : TimeZoneFormat -> TimeZone -> String
timeZoneOffsetString' fmt tz = let
  h = tz.minutes `div` 60
  m = tz.minutes `mod` 60
  in case fmt.sep of
          Nothing => format' {width:=fmt.width, pad:=fmt.pad } $ h * 100 + m
          Just c  => format' {width:=map (\w => max 0 (w `minus` 3)) fmt.width,
                              pad:=fmt.pad} h
                     ++ format' {width:=Just 2, pad:=Just '0'} m

export timeZoneOffsetString : TimeZone -> String
timeZoneOffsetString x = timeZoneOffsetString' defaultTimeZoneFormat x


||| This only shows the time zone name, or offset if the name is empty.
public export
Show TimeZone where
  show x = if x.name == "" then timeZoneOffsetString x else x.name


||| Create a nameless non-summer timezone for this number of minutes.
export minutesToTimeZone : Int -> TimeZone
minutesToTimeZone m = MkTimeZone m False ""


||| Create a nameless non-summer timezone for this number of hours.
export hoursToTimeZone : Int -> TimeZone
hoursToTimeZone i = minutesToTimeZone (60 * i)


||| The UTC time zone.
export utc : TimeZone
utc = MkTimeZone 0 False "UTC"


{-

toCTime :: Int64 -> IO CTime
toCTime t = let
    tt = fromIntegral t
    t' = fromIntegral tt
    -- there's no instance Bounded CTime, so this is the easiest way to check for overflow
    in if t' == t
           then return $ CTime tt
           else fail "Data.Time.LocalTime.Internal.TimeZone.toCTime: Overflow"

-- | Get the local time-zone for a given time (varying as per summertime adjustments).
getTimeZoneSystem :: SystemTime -> IO TimeZone
getTimeZoneSystem t = do
    ctime <- toCTime $ systemSeconds t
    getTimeZoneCTime ctime

-- | Get the local time-zone for a given time (varying as per summertime adjustments).
getTimeZone :: UTCTime -> IO TimeZone
getTimeZone t = do
    ctime <- toCTime $ floor $ utcTimeToPOSIXSeconds t
    getTimeZoneCTime ctime
-}


-- --------------------------------------------------------------------------
-- vim: tw=80 sw=2 expandtab :

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

**English** | [日本語](README.ja.md)

# idris2-time-calendar-clock

A comprehensive date, time, and calendar library for Idris 2, ported from and inspired by Haskell's standard [`time`](https://hackage.haskell.org/package/time) package.

This library provides full calendar arithmetic based on Modified Julian Day (MJD), picosecond-resolution ($10^{-12}$ s) duration calculations, timezone-aware civil time (`LocalTime` / `ZonedTime`), and multi-backend system clock access.

---

## Features

- **Picosecond Resolution**: Built on top of [`idris2-missing-numeric`](https://github.com/seagull-kamome/idris2-missing-numeric)'s decimal fixed-point type `Fixed 12`, eliminating floating-point rounding errors in duration and difference calculations.
- **Comprehensive Calendar Arithmetic**:
  - Proleptic Gregorian Calendar (with leap year calculations, month clipping, and rolling over)
  - Proleptic Julian Calendar
  - ISO 8601 Week Date format (Year, Week Number, Day of Week)
  - Ordinal Date format (Year, Day of Year from 1 to 366)
- **Distinct Time Scales**:
  - **`UTCTime`**: Coordinated Universal Time
  - **`UniversalTime` (UT1)**: Astronomical time based on the Earth's rotation (using exact `Rational` representation)
  - **`POSIXTime`**: Seconds elapsed since 1970-01-01 00:00:00 UTC (UNIX epoch)
  - **`AbsoluteTime`**: International Atomic Time (TAI) scale independent of leap seconds
- **Time Zones & Civil Time**:
  - Full support for `TimeZone`, `TimeOfDay` (hours, minutes, seconds), `LocalTime`, and `ZonedTime`.
- **Multi-Backend Real-Time Clock**:
  - Cross-platform FFI implementations for Chez Scheme, JavaScript / Node, RefC, and RC2 (`getSystemTime`, `getSystemLocalTime`).

---

## Module Overview

| Module | Types & Core Functions | Description |
|---|---|---|
| [`Data.Time.Calendar`](src/Data/Time/Calendar.idr) | `Day`, `addDays`, `diffDays` | Core date representation based on Modified Julian Day (MJD) |
| [`Data.Time.Calendar.Gregorian`](src/Data/Time/Calendar/Gregorian.idr) | `toGregorian`, `fromGregorian`, `isLeapYear` | Gregorian calendar conversions, leap years, month length, clipped month addition |
| [`Data.Time.Calendar.WeekDate`](src/Data/Time/Calendar/WeekDate.idr) | `DayOfWeek`, `dayOfWeek`, `toWeekDate` | Days of week (Mon–Sun) and ISO 8601 week number calculations |
| [`Data.Time.Calendar.OrdinalDate`](src/Data/Time/Calendar/OrdinalDate.idr) | `toOrdinalDate`, `fromOrdinalDate` | Conversions to and from day-of-year (1–366) |
| [`Data.Time.Calendar.Julian`](src/Data/Time/Calendar/Julian.idr) | `toJulianYearAndDay`, `fromJulianYearAndDay` | Proleptic Julian calendar support |
| [`Data.Time.Clock`](src/Data/Time/Clock.idr) | `UniversalTime`, `UTCTime`, `DiffTime` | Time definitions, conversions, and duration operations |
| [`Data.Time.Clock.DiffTime`](src/Data/Time/Clock/DiffTime.idr) | `DiffTime`, `NominalDiffTime` | Picosecond-precision (`Fixed 12`) time differences and durations |
| [`Data.Time.Clock.POSIX`](src/Data/Time/Clock/POSIX.idr) | `POSIXTime`, `posixEpochDay` | UNIX timestamp representation and bidirectional conversions with `UTCTime` |
| [`Data.Time.Clock.System`](src/Data/Time/Clock/System.idr) | `getSystemTime`, `systemToUTCTime` | Query OS system clock with nanosecond resolution |
| [`Data.Time.LocalTime`](src/Data/Time/LocalTime.idr) | `LocalTime`, `TimeOfDay`, `TimeZone`, `ZonedTime` | Local time, timezone offsets, and time differences |

---

## Installation & Dependencies

### Requirements
- **Idris 2** >= 0.8.0
- **Dependencies**:
  - `missing-numeric` >= 0.7.0
  - `contrib` >= 0.8.0
  - `elab-util` >= 0.6.0
  - `sop` >= 0.6.0

### Using Pack
Add the following entries to your `pack.toml`:

```toml
[custom.all.missing-numeric]
type = "github"
url  = "https://github.com/seagull-kamome/idris2-missing-numeric.git"
commit = "master"
ipkg = "missing-numeric.ipkg"

[custom.all.time-calendar-clock]
type = "github"
url  = "https://github.com/seagull-kamome/idris2-time-calendar-clock.git"
commit = "master"
ipkg = "time-calendar-clock.ipkg"
```

### Manual Build
```bash
# Build package
idris2 --build time-calendar-clock.ipkg

# Install package
idris2 --install time-calendar-clock.ipkg

# Run tests
make check
```

---

## Usage & Examples

### 1. Date & Calendar Arithmetic (`Data.Time.Calendar`)

```idris
import Data.Time.Calendar

exampleCalendar : IO ()
exampleCalendar = do
  -- Construct date from Gregorian calendar (year, month, day)
  let today = fromGregorian 2026 11 3
  putStrLn $ "Today (ISO 8601): " ++ show today  -- "2026-11-03"

  -- Deconstruct year, month, day
  let (year, month, day) = toGregorian today
  putStrLn $ "Year: " ++ show year ++ ", Month: " ++ show month ++ ", Day: " ++ show day

  -- Check leap year and month length
  putStrLn $ "Is 2026 leap year?: " ++ show (isLeapYear 2026)             -- False
  putStrLn $ "Days in Feb 2024: " ++ show (gregorianMonthLength 2024 2)   -- 29

  -- Day of the week
  putStrLn $ "Day of week: " ++ show (dayOfWeek today)                    -- Tuesday

  -- Add days and compute difference
  let nextWeek = addDays 7 today
  putStrLn $ "Next week: " ++ show nextWeek                               -- "2026-11-10"
  putStrLn $ "Diff days: " ++ show (diffDays nextWeek today)              -- 7
```

### 2. System Clock & UTC (`Data.Time.Clock`)

```idris
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Clock.POSIX

exampleClock : IO ()
exampleClock = do
  -- Read real-time clock from OS
  sysTime <- getSystemTime
  
  -- Convert to UTCTime
  let utcNow = systemToUTCTime sysTime
  putStrLn $ "Current UTC: " ++ show utcNow

  -- Convert to UNIX timestamp (POSIXTime)
  let posix = (cast utcNow : POSIXTime)
  putStrLn $ "POSIX Timestamp (seconds): " ++ show posix.seconds

  -- Add durations (e.g., 3600 seconds = 1 hour)
  let oneHourLater = addUTCTime (fromInteger 3600) utcNow
  putStrLn $ "1 hour later: " ++ show oneHourLater

  -- Measure elapsed difference (NominalDiffTime)
  let elapsed = diffUTCTime oneHourLater utcNow
  putStrLn $ "Elapsed: " ++ show elapsed
```

### 3. Time Zones & Local Civil Time (`Data.Time.LocalTime`)

```idris
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.LocalTime

exampleLocalTime : IO ()
exampleLocalTime = do
  -- Japan Standard Time (JST = UTC+9 hours = +540 minutes)
  let jst = MkTimeZone 540 False "JST"
  
  -- Convert UTC to local civil time
  sysTime <- getSystemTime
  let utcNow = systemToUTCTime sysTime
  let jstNow = utcToLocalTime jst utcNow
  putStrLn $ "JST LocalTime: " ++ show jstNow  -- "2026-11-03 14:30:00.000000000000"

  -- Zoned time (LocalTime with TimeZone attached)
  let zoned = utcToZonedTime jst utcNow
  putStrLn $ "Zoned Time: " ++ show zoned      -- "2026-11-03 14:30:00.000000000000  JST"

  -- Directly query system local time
  sysLocal <- getSystemLocalTime
  putStrLn $ "System Local Time: " ++ show sysLocal
```

---

## Architecture & Design Decisions

### Why `Fixed 12`?
Standard IEEE 754 floating-point numbers (`Double`) introduce accumulated rounding errors when performing repeated duration arithmetic.
Following Haskell's `Pico` ($10^{-12}$ s) design, this library uses `missing-numeric`'s `Fixed 12` to guarantee exact picosecond arithmetic without loss of precision.

### Clear Separation of Time Scales
- **`Day`**: Pure calendar date with no time of day.
- **`UniversalTime` (UT1)**: Solar time based on Earth's rotation, ideal for astronomical calculations.
- **`UTCTime`**: Everyday atomic-referenced standard civil time.
- **`POSIXTime`**: Standard UNIX timestamp assuming exactly 86,400 seconds per nominal day.
- **`LocalTime` / `ZonedTime`**: Human-readable civil time incorporating local time zone offsets and daylight saving adjustments.

---

## License

This project is licensed under the [MIT License](LICENSE).

**Author**: HATTORI, Hiroki ([seagull.kamome@gmail.com](mailto:seagull.kamome@gmail.com))

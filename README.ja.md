[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

[English](README.md) | **日本語**

# idris2-time-calendar-clock

Haskell の標準ライブラリ [`time`](https://hackage.haskell.org/package/time) パッケージをベースに移植・設計された、Idris2 向けの高機能な日付・時刻・暦（カレンダー）ライブラリです。

修正ユリウス日（MJD）を基礎とする各種カレンダー計算、ピコ秒精度（$10^{-12}$ 秒）の時刻差・所要時間表現、タイムゾーンを考慮した現地時刻（LocalTime / ZonedTime）、および複数バックエンドに対応したシステムクロック（実時間取得）を提供します。

---

## 特徴

- **ピコ秒精度の時刻表現**: [`idris2-missing-numeric`](https://github.com/seagull-kamome/idris2-missing-numeric) の固定小数点数 `Fixed 12` を基盤に採用し、浮動小数点誤差のない正確な所要時間・時刻差を計算可能。
- **包括的な暦（カレンダー）サポート**:
  - 先発グレゴリオ暦（Proleptic Gregorian Calendar）
  - 先発ユリウス暦（Proleptic Julian Calendar）
  - ISO 8601 週日付（Week Date: 年、週番号、曜日）
  - 年間通算日（Ordinal Date: 年、元日からの通算日数）
- **複数の時刻スケール**:
  - **`UTCTime`**: 協定世界時（原子時基準の標準時）
  - **`UniversalTime` (UT1)**: 地球の自転に基づく天文時（`Rational` 表現）
  - **`POSIXTime`**: 1970-01-01 00:00:00 UTC からの経過秒数（UNIX時間）
  - **`AbsoluteTime`**: うるう秒に左右されない国際原子時（TAI）
- **タイムゾーンと現地時刻**:
  - `TimeZone`, `TimeOfDay` (時・分・秒), `LocalTime`, `ZonedTime` をサポート。
- **マルチバックエンド対応のシステムクロック**:
  - Chez Scheme, JavaScript / Node, RefC, RC2 各バックエンドに対応した `getSystemTime` / `getSystemLocalTime` FFI を実装。

---

## モジュール構成

| モジュール | 型・主な関数 | 説明 |
|---|---|---|
| [`Data.Time.Calendar`](src/Data/Time/Calendar.idr) | `Day`, `addDays`, `diffDays` | 修正ユリウス日（MJD）に基づく日付の基本表現と日数加減算 |
| [`Data.Time.Calendar.Gregorian`](src/Data/Time/Calendar/Gregorian.idr) | `toGregorian`, `fromGregorian`, `isLeapYear` | グレゴリオ暦変換、うるう年判定、月末クリップ付き年月加算 |
| [`Data.Time.Calendar.WeekDate`](src/Data/Time/Calendar/WeekDate.idr) | `DayOfWeek`, `dayOfWeek`, `toWeekDate` | 曜日計算（月〜日）、ISO 8601 週番号計算 |
| [`Data.Time.Calendar.OrdinalDate`](src/Data/Time/Calendar/OrdinalDate.idr) | `toOrdinalDate`, `fromOrdinalDate` | 年および元日からの通算日（1〜366日）の変換 |
| [`Data.Time.Calendar.Julian`](src/Data/Time/Calendar/Julian.idr) | `toJulianYearAndDay`, `fromJulianYearAndDay` | ユリウス暦の計算と変換 |
| [`Data.Time.Clock`](src/Data/Time/Clock.idr) | `UniversalTime`, `UTCTime`, `DiffTime` | 各種時刻表現、所要時間の計算 |
| [`Data.Time.Clock.DiffTime`](src/Data/Time/Clock/DiffTime.idr) | `DiffTime`, `NominalDiffTime` | ピコ秒精度（`Fixed 12`）の所要時間および公称時間差 |
| [`Data.Time.Clock.POSIX`](src/Data/Time/Clock/POSIX.idr) | `POSIXTime`, `posixEpochDay` | UNIXエポック時間表現と `UTCTime` との相互変換 |
| [`Data.Time.Clock.System`](src/Data/Time/Clock/System.idr) | `getSystemTime`, `systemToUTCTime` | OSの現在時刻取得（ナノ秒分解能） |
| [`Data.Time.LocalTime`](src/Data/Time/LocalTime.idr) | `LocalTime`, `TimeOfDay`, `TimeZone`, `ZonedTime` | タイムゾーンオフセット付き時刻、時刻の加減算 |

---

## インストールと依存関係

### 要件
- **Idris2** >= 0.8.0
- **依存パッケージ**:
  - `missing-numeric` >= 0.7.0
  - `contrib` >= 0.8.0
  - `elab-util` >= 0.6.0
  - `sop` >= 0.6.0

### pack を使用する場合
`pack.toml` に以下を追加します：

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

### 手動ビルドする場合
```bash
# パッケージのビルド
idris2 --build time-calendar-clock.ipkg

# インストール
idris2 --install time-calendar-clock.ipkg

# テストの実行
make check
```

---

## 使い方とコード例

### 1. 日付とカレンダー計算 (`Data.Time.Calendar`)

```idris
import Data.Time.Calendar

exampleCalendar : IO ()
exampleCalendar = do
  -- グレゴリオ暦から日付を生成 (年 月 日)
  let today = fromGregorian 2026 11 3
  putStrLn $ "Today (ISO 8601): " ++ show today  -- "2026-11-03"

  -- 年・月・日の取り出し
  let (year, month, day) = toGregorian today
  putStrLn $ "Year: " ++ show year ++ ", Month: " ++ show month ++ ", Day: " ++ show day

  -- うるう年判定・月の日数
  putStrLn $ "Is 2026 leap year?: " ++ show (isLeapYear 2026)             -- False
  putStrLn $ "Days in Feb 2024: " ++ show (gregorianMonthLength 2024 2)   -- 29

  -- 曜日の取得
  putStrLn $ "Day of week: " ++ show (dayOfWeek today)                    -- Tuesday

  -- 日数の加算と日付差
  let nextWeek = addDays 7 today
  putStrLn $ "Next week: " ++ show nextWeek                               -- "2026-11-10"
  putStrLn $ "Diff days: " ++ show (diffDays nextWeek today)              -- 7
```

### 2. 現在時刻の取得と UTC (`Data.Time.Clock`)

```idris
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Clock.POSIX

exampleClock : IO ()
exampleClock = do
  -- OSの現在時刻を取得
  sysTime <- getSystemTime
  
  -- UTCTime（UTC日時）へ変換
  let utcNow = systemToUTCTime sysTime
  putStrLn $ "Current UTC: " ++ show utcNow

  -- UNIXタイムスタンプ（POSIXTime）との相互変換
  let posix = (cast utcNow : POSIXTime)
  putStrLn $ "POSIX Timestamp (seconds): " ++ show posix.seconds

  -- 所要時間の加算（例: 3600秒 = 1時間を加算）
  let oneHourLater = addUTCTime (fromInteger 3600) utcNow
  putStrLn $ "1 hour later: " ++ show oneHourLater

  -- 2つの時刻の差分（NominalDiffTime）
  let elapsed = diffUTCTime oneHourLater utcNow
  putStrLn $ "Elapsed: " ++ show elapsed
```

### 3. タイムゾーンと現地時刻 (`Data.Time.LocalTime`)

```idris
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.LocalTime

exampleLocalTime : IO ()
exampleLocalTime = do
  -- 日本標準時 (JST = UTC+9時間 = +540分)
  let jst = MkTimeZone 540 False "JST"
  
  -- UTC時刻から現地時刻への変換
  sysTime <- getSystemTime
  let utcNow = systemToUTCTime sysTime
  let jstNow = utcToLocalTime jst utcNow
  putStrLn $ "JST LocalTime: " ++ show jstNow  -- "2026-11-03 14:30:00.000000000000"

  -- タイムゾーン付き時刻 (ZonedTime)
  let zoned = utcToZonedTime jst utcNow
  putStrLn $ "Zoned Time: " ++ show zoned      -- "2026-11-03 14:30:00.000000000000  JST"

  -- システムのローカルタイムを直接取得
  sysLocal <- getSystemLocalTime
  putStrLn $ "System Local Time: " ++ show sysLocal
```

---

## アーキテクチャと設計思想

### なぜ `Fixed 12` なのか？
多くの言語やライブラリでは秒の端数に IEEE 754 浮動小数点数（`Double`）を使用しますが、微小な所要時間の累積演算で丸め誤差や桁落ちが発生します。
本ライブラリでは Haskell の `Pico`（$10^{-12}$ 秒）設計を踏襲し、`missing-numeric` の固定小数点数 `Fixed 12` を採用することで、厳密なピコ秒分解能と代数的安全性を確保しています。

### 複数の時刻スケールの明確な分離
- **`Day`**: カレンダー上の日付のみを表す（時刻情報を持たない）。
- **`UniversalTime` (UT1)**: 地球の自転を基準とした真太陽時。天文学的計算に適しています。
- **`UTCTime`**: 日常的なコンピュータ上の標準時刻（うるう秒を含む日長に対応）。
- **`POSIXTime`**: 各日を一律86400秒とみなすUNIX標準タイムスタンプ。
- **`LocalTime` / `ZonedTime`**: 人間社会の地域ルール（時差・サマータイム）を反映した市民時。

---

## ライセンス

本プロジェクトは [MIT License](LICENSE) の下で公開されています。

**作者**: HATTORI, Hiroki ([seagull.kamome@gmail.com](mailto:seagull.kamome@gmail.com))

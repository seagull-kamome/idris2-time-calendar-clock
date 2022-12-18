module Main

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.System

%default total


partial main : IO ()
main = do
  do
    t <- getSystemLocalTime
    putStrLn $ "getSystemLocalTime : " ++ show t

  do
    t <- getSystemTime
    putStrLn $ "systemEpochDay : " ++ show systemEpochDay
    putStrLn $ "getSystemTIme : " ++ show t
    putStrLn $ "  systemToUTCTime : " ++ show (systemToUTCTime t)
    putStrLn $ "    <-> utcToSystemTime : " ++ show (utcToSystemTime (systemToUTCTime t))
    putStrLn $ "  truncateSystemTimeLeapSecond : " ++ show (truncateSystemTimeLeapSecond t)
    putStrLn $ show $ dayOfWeek $ fromGregorian 2021 8 21
    putStrLn $ show $ (modifiedJulianDay $ fromGregorian 2021 8 21) `mod` 7





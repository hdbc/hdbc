{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

module TestSqlValue where
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property (Result)
import qualified Test.HUnit as HU
import Database.HDBC
import Data.Time.Format (parseTime)
import Data.Time.LocalTime
import Database.HDBC.Locale (defaultTimeLocale, iso8601DateFormat, oldIso8601DateFormat)
import Data.Maybe

instance Eq ZonedTime where
    a == b = zonedTimeToUTC a == zonedTimeToUTC b &&
             zonedTimeZone a == zonedTimeZone b

toSql_Int :: Int -> Property
toSql_Int x = toSql x === SqlInt64 (fromIntegral x)

fromSql_Int :: Int -> Property
fromSql_Int x = 
    Right x === safeFromSql (SqlInt64 (fromIntegral x))

testZonedTimeStr = "1989-08-01 15:33:01 -0500"
testZonedTime :: ZonedTime
testZonedTime = fromJust $ parseTime defaultTimeLocale (iso8601DateFormat (Just "%T %z"))
                testZonedTimeStr

testZonedTimeFracStr = "1989-08-01 15:33:01.536 -0500"
testZonedTimeFrac :: ZonedTime
testZonedTimeFrac = fromJust $ parseTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q %z"))
                    testZonedTimeFracStr

testZonedTimeTwoDigitYearStr = "89-08-01 15:33:01 -0500"
testZonedTimeTwoDigitYear :: ZonedTime
testZonedTimeTwoDigitYear = fromJust $ parseTime defaultTimeLocale (oldIso8601DateFormat (Just "%T %z"))
                            testZonedTimeTwoDigitYearStr

ztparsenf =
    (HU.@=?) testZonedTime (fromSql (SqlString testZonedTimeStr))
ztparsef =
    (HU.@=?) testZonedTimeFrac (fromSql (SqlString testZonedTimeFracStr))
ztparseTwoDigitYear =
    (HU.@=?) testZonedTimeTwoDigitYear (fromSql (SqlString testZonedTimeTwoDigitYearStr))

hut msg = HU.TestLabel msg . HU.TestCase

allHUnitTests = [hut "non-frac parse" ztparsenf,
                 hut "frac parse" ztparsef,
                 hut "old serialization of two digit year without leading zeros" ztparseTwoDigitYear
                ]

allQuickCheckTests = [toSql_Int,
                      fromSql_Int
                     ]

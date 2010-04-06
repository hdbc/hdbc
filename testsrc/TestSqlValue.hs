{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

module TestSqlValue where
import TestInfrastructure
import Test.QuickCheck
import Test.QuickCheck.Tools
import qualified Test.HUnit as HU
import Database.HDBC
import Data.Time.Format
import Data.Time.LocalTime
import Database.HDBC.Locale (defaultTimeLocale, iso8601DateFormat)
import Data.Maybe

instance Eq ZonedTime where
    a == b = zonedTimeToUTC a == zonedTimeToUTC b &&
             zonedTimeZone a == zonedTimeZone b

toSql_Int :: Int -> Result
toSql_Int x = toSql x @?= SqlInt32 (fromIntegral x)

fromSql_Int :: Int -> Result
fromSql_Int x = 
    Right x @=? safeFromSql (SqlInt32 (fromIntegral x))

testZonedTimeStr = "1989-08-01 15:33:01 -0500"
testZonedTime :: ZonedTime
testZonedTime = fromJust $ parseTime defaultTimeLocale (iso8601DateFormat (Just "%T %z"))
                testZonedTimeStr

testZonedTimeFracStr = "1989-08-01 15:33:01.536 -0500"
testZonedTimeFrac :: ZonedTime
testZonedTimeFrac = fromJust $ parseTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q %z"))
                    testZonedTimeFracStr

ztparsenf =
    (HU.@=?) testZonedTime (fromSql (SqlString testZonedTimeStr))
ztparsef =
    (HU.@=?) testZonedTimeFrac (fromSql (SqlString testZonedTimeFracStr))

hut msg = HU.TestLabel msg . HU.TestCase

allt = [q "toSql Int" toSql_Int,
        q "safeFromSql Int" fromSql_Int,
        hut "non-frac parse" ztparsenf,
        hut "frac parse" ztparsef]

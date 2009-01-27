{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

module TestSqlValue where
import TestInfrastructure
import Test.QuickCheck
import Test.QuickCheck.Tools
import Database.HDBC

toSql_Int :: Int -> Result
toSql_Int x = toSql x @?= SqlInt32 (fromIntegral x)

fromSql_Int :: Int -> Result
fromSql_Int x = 
    Right x @=? safeFromSql (SqlInt32 (fromIntegral x))

allt = [q "toSql Int" toSql_Int,
        q "safeFromSql Int" fromSql_Int]

{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

module TestSqlValue where
import TestInfrastructure
import Database.HDBC.SqlValue

propInt :: Int -> Result
propInt x = toSql x @?= SqlInt x

allt = [q "toSql Int" propInt]

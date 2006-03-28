{-
Copyright (C) 2006 John Goerzen <jgoerzen@complete.org>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}

{- |
   Module     : Database.HDBC.ColTypes
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU LGPL, version 2.1 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Definitions of, and utilities for, specifying what type of data is represented
by a column.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Database.HDBC.ColTypes (SqlColDesc(..),
                               SqlTypeId(..),
                               SqlInterval(..)
                              )

where
import Data.Dynamic

{- | The description of a column.

Fields are Nothing if the database backend cannot supply the
requested information.

The colSize field works like this:

For character types, the maximum width of the column.  For numeric
types, the total number of digits allowed.  See the ODBC manual for more.

The colOctetLength field is defined for character and binary types, and
gives the number of bytes the column requires, regardless of encoding.
-}
data SqlColDesc = 
   SqlColDesc {
               colType :: SqlTypeId   -- ^ Type of data stored here
              ,colSize :: Maybe Int   -- ^ The size of a column
              ,colOctetLength :: Maybe Int -- ^ The maximum size in octets
              ,colDecDigits :: Maybe Int -- ^ Digits to the right of the period
              ,colNullable :: Maybe Bool -- ^ Whether NULL is acceptable
              }
   deriving (Eq, Read, Show)

sqlColDescTc :: TyCon
sqlColDescTc = mkTyCon "Database.HDBC.SqlColDesc"

instance Typeable SqlColDesc where
    typeOf _ = mkTyConApp sqlColDescTc []

{- | The type identifier for a given column. 

This represents the type of data stored in the column in the underlying
SQL engine.  It does not form the entire column type; see 'SqlColDesc' for
that.

These types correspond mainly to those defined by ODBC. -}
data SqlTypeId =
    SqlCharT                    -- ^ Fixed-width character strings
    | SqlVarCharT               -- ^ Variable-width character strings
    | SqlLongVarCharT           -- ^ Variable-width character strings, max length implementation dependant
    | SqlWCharT                 -- ^ Fixed-width Unicode strings
    | SqlWVarCharT              -- ^ Variable-width Unicode strings
    | SqlWLongVarCharT          -- ^ Variable-width Unicode strings, max length implementation dependant
    | SqlDecimalT               -- ^ Signed exact values
    | SqlNumericT               -- ^ Signed exact integer values
    | SqlSmallIntT              -- ^ 16-bit integer values
    | SqlIntegerT               -- ^ 32-bit integer values
    | SqlRealT
    | SqlFloatT                 -- ^ Signed inexact floating-point values
    | SqlDoubleT                -- ^ Signed inexact double-precision values
    | SqlBitT                   -- ^ A single bit
    | SqlTinyIntT               -- ^ 8-bit integer values
    | SqlBigIntT                -- ^ 64-bit integer values
    | SqlBinaryT                -- ^ Fixed-length binary data
    | SqlVarBinaryT             -- ^ Variable-length binary data
    | SqlLongVarBinaryT         -- ^ Variable-length binary data, max length implementation dependant
    | SqlDateT                  -- ^ A date
    | SqlTimeT                  -- ^ A time
    | SqlTimestampT             -- ^ Combined date and time
    | SqlUTCDateTimeT           -- ^ UTC date\/time
    | SqlUTCTimeT               -- ^ UTC time
    | SqlIntervalT SqlInterval  -- ^ A time or date difference
    | SqlGUIDT                  -- ^ Global unique identifier
    | SqlUnknownT String        -- ^ A type not represented here; implementation-specific information in the String

  deriving (Eq, Show, Read)

sqlTypeIdTc :: TyCon
sqlTypeIdTc = mkTyCon "Database.HDBC.SqlTypeId"

instance Typeable SqlTypeId where
    typeOf _ = mkTyConApp sqlTypeIdTc []

{- | The different types of intervals in SQL. -}
data SqlInterval =
      SqlIntervalMonthT         -- ^ Difference in months
    | SqlIntervalYearT          -- ^ Difference in years
    | SqlIntervalYearToMonthT   -- ^ Difference in years+months
    | SqlIntervalDayT           -- ^ Difference in days
    | SqlIntervalHourT          -- ^ Difference in hours
    | SqlIntervalMinuteT        -- ^ Difference in minutes
    | SqlIntervalSecondT        -- ^ Difference in seconds
    | SqlIntervalDayToHourT     -- ^ Difference in days+hours
    | SqlIntervalDayToMinuteT   -- ^ Difference in days+minutes
    | SqlIntervalDayToSecondT   -- ^ Difference in days+seconds
    | SqlIntervalHourToMinuteT  -- ^ Difference in hours+minutes
    | SqlIntervalHourToSecondT  -- ^ Difference in hours+seconds
    | SqlIntervalMinuteToSecondT -- ^ Difference in minutes+seconds
      deriving (Eq, Show, Read)

sqlIntervalTc :: TyCon
sqlIntervalTc = mkTyCon "Database.HDBC.SqlInterval"

instance Typeable SqlInterval where
    typeOf _ = mkTyConApp sqlIntervalTc []

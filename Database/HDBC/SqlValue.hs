{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
module Database.HDBC.SqlValue
    (
     -- * SQL value marshalling
     SqlValue(..),
     safeFromSql, toSql, fromSql,
     nToSql, iToSql, posixToSql
    )

where
import Data.Dynamic
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Char(ord,toUpper)
import Data.Word
import Data.Int
import qualified System.Time as ST
import Data.Time
import Data.Time.Clock.POSIX
import Database.HDBC.Locale (defaultTimeLocale, iso8601DateFormat)
import Data.Ratio
import Data.Convertible
import Data.Fixed
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

quickError :: (Typeable a, Convertible SqlValue a) => SqlValue -> ConvertResult a
quickError sv = convError "incompatible types" sv
  
{- | Convert a value to an 'SqlValue'.  This function is simply
a restricted-type wrapper around 'convert'.  See extended notes on 'SqlValue'. -}
toSql :: Convertible a SqlValue => a -> SqlValue
toSql = convert

{- | Conversions to and from 'SqlValue's and standard Haskell types.

This function converts from an 'SqlValue' to a Haskell value.  Many people will use the simpler
   'fromSql' instead.  This function is simply a restricted-type wrapper around
   'safeConvert'. -}
safeFromSql :: Convertible SqlValue a => SqlValue -> ConvertResult a
safeFromSql = safeConvert

{- | Convert from an 'SqlValue' to a Haskell value.  Any problem is indicated by
   calling 'error'.  This function is simply a restricted-type wrapper around
   'convert'.  See extended notes on 'SqlValue'. -}
fromSql :: Convertible SqlValue a => SqlValue -> a
fromSql = convert

{- | Converts any Integral type to a 'SqlValue' by using toInteger. -}
nToSql :: Integral a => a -> SqlValue
nToSql n = SqlInteger (toInteger n)

{- | Convenience function for using numeric literals in your program. -}
iToSql :: Int -> SqlValue
iToSql = toSql

{- | Convenience function for converting 'POSIXTime' to a 'SqlValue', because
'toSql' cannot do the correct thing in this instance. -}
posixToSql :: POSIXTime -> SqlValue
posixToSql x = SqlPOSIXTime x

{- | 'SqlValue' is he main type for expressing Haskell values to SQL databases.

/INTRODUCTION TO SQLVALUE/

This type is used to marshall Haskell data to and from database APIs.
HDBC driver interfaces will do their best to use the most accurate and
efficient way to send a particular value to the database server.

Values read back from the server are constructed with the most appropriate 'SqlValue'
constructor.  'fromSql' or 'safeFromSql' 
can then be used to convert them into whatever type
is needed locally in Haskell.

Most people will use 'toSql' and 'fromSql' instead of manipulating
'SqlValue's directly.

/EASY CONVERSIONS BETWEEN HASKELL TYPES/

Conversions are powerful; for instance, you can call 'fromSql' on a SqlInt32
and get a String or a Double out of it.  This class attempts to Do
The Right Thing whenever possible, and will raise an error when asked to
do something incorrect.  In particular, when converting to any type
except a Maybe, 'SqlNull' as the input will cause an error to be raised.

Conversions are implemented in terms of the "Data.Convertible" module, part of the
convertible package.  You can refer to its documentation, and import that module,
if you wish to parse the Left result from 'safeFromSql' yourself, or write your
own conversion instances.

Here are some notes about conversion:

 * Fractions of a second are not preserved on time values

 * There is no @safeToSql@ because 'toSql' never fails.

See also 'toSql', 'safeFromSql', 'fromSql', 'nToSql', 'iToSql', 'posixToSql'.

/ERROR CONDITIONS/

There may sometimes be an error during conversion.  For instance, if you have a
'SqlString' and are attempting to convert it to an Integer, but it doesn't parse as
an Integer, you will get an error.  This will be indicated as an exception if using
'fromSql', or a Left result if using 'safeFromSql'.

/SPECIAL NOTE ON POSIXTIME/

Note that a 'NominalDiffTime' or 'POSIXTime' is converted to 'SqlDiffTime' by
'toSql'.  HDBC cannot differentiate between 'NominalDiffTime' and 'POSIXTime'
since they are the same underlying type.  You must construct 'SqlPOSIXTime'
manually or via 'posixToSql', or use 'SqlUTCTime'.

/DETAILS ON SQL TYPES/

HDBC database backends are expected to marshal date and time data back and
forth using the appropriate representation for the underlying database engine.
Databases such as PostgreSQL with builtin date and time types should see automatic
conversion between these Haskell types to database types.  Other databases will be
presented with an integer or a string.  Care should be taken to use the same type on
the Haskell side as you use on the database side.  For instance, if your database
type lacks timezone information, you ought not to use ZonedTime, but
instead LocalTime or UTCTime.  Database type systems are not always as rich
as Haskell.  For instance, for data stored in a TIMESTAMP
WITHOUT TIME ZONE column, HDBC may not be able to tell if it is intended
as UTCTime or LocalTime data, and will happily convert it to both, 
upon your request.  It is
your responsibility to ensure that you treat timezone issues with due care.

This behavior also exists for other types.  For instance, many databases do not
have a Rational type, so they will just use the show function and
store a Rational as a string.

The conversion between Haskell types and database types is complex,
and generic code in HDBC or its backends cannot possibly accomodate
every possible situation.  In some cases, you may be best served by converting your
Haskell type to a String, and passing that to the database.

/UNICODE AND BYTESTRINGS/

Beginning with HDBC v2.0, interactions with a database are presumed to occur in UTF-8.

To accomplish this, whenever a ByteString must be converted to or from a String,
the ByteString is assumed to be in UTF-8 encoding, and will be decoded or encoded
as appropriate.  Database drivers will generally present text or string data they have
received from the database as a SqlValue holding a ByteString, which 'fromSql' will
automatically convert to a String, and thus automatically decode UTF-8, when
you need it.  In the other direction, database drivers will generally convert
a 'SqlString' to a ByteString in UTF-8 encoding before passing it to the
database engine.

If you are handling some sort of binary data that is not in UTF-8, you can of course
work with the ByteString directly, which will bypass any conversion.

Due to lack of support by database engines, lazy ByteStrings are not passed to database
drivers.  When you use 'toSql' on a lazy ByteString, it will be converted to a strict
ByteString for storage.  Similarly, 'fromSql' will convert a strict ByteString to
a lazy ByteString if you demand it.

/EQUALITY OF SQLVALUE/

Two SqlValues are considered to be equal if one of these hold.  The
first comparison that can be made is controlling; if none of these
comparisons can be made, then they are not equal:

 * Both are NULL

 * Both represent the same type and the encapsulated values are considered equal
   by applying (==) to them

 * The values of each, when converted to a string, are equal.

/STRING VERSIONS OF TIMES/

Default string representations are given as comments below where such are non-obvious.
These are used for 'fromSql' when a 'String' is desired.  They are also defaults for
representing data to SQL backends, though individual backends may override them
when a different format is demanded by the underlying database.  Date and time formats
use ISO8601 date format, with HH:MM:SS added for time, and -HHMM added for timezone
offsets.

/DEPRECATED CONSTRUCTORS/

'SqlEpochTime' and 'SqlTimeDiff' are no longer created automatically by any
'toSql' or 'fromSql' functions or database backends.  They may still be manually
constructed, but are
expected to be removed in a future version.  Although these two constructures will
be removed, support for marshalling to and from the old System.Time data will be
maintained as long as System.Time is, simply using the newer data types for conversion.
-}
data SqlValue = SqlString String 
              | SqlByteString B.ByteString
              | SqlWord32 Word32
              | SqlWord64 Word64
              | SqlInt32 Int32
              | SqlInt64 Int64
              | SqlInteger Integer
              | SqlChar Char
              | SqlBool Bool
              | SqlDouble Double
              | SqlRational Rational
              | SqlLocalDate Day            -- ^ Local YYYY-MM-DD (no timezone)
              | SqlLocalTimeOfDay TimeOfDay -- ^ Local HH:MM:SS (no timezone)
              | SqlZonedLocalTimeOfDay TimeOfDay TimeZone -- ^ Local HH:MM:SS -HHMM.  Converts to and from (TimeOfDay, TimeZone).
              | SqlLocalTime LocalTime      -- ^ Local YYYY-MM-DD HH:MM:SS (no timezone)
              | SqlZonedTime ZonedTime      -- ^ Local YYYY-MM-DD HH:MM:SS -HHMM.  Considered equal if both convert to the same UTC time.
              | SqlUTCTime UTCTime          -- ^ UTC YYYY-MM-DD HH:MM:SS
              | SqlDiffTime NominalDiffTime -- ^ Calendar diff between seconds.  Rendered as Integer when converted to String, but greater precision may be preserved for other types or to underlying database.
              | SqlPOSIXTime POSIXTime      -- ^ Time as seconds since midnight Jan 1 1970 UTC.  Integer rendering as for 'SqlDiffTime'.
              | SqlEpochTime Integer      -- ^ DEPRECATED Representation of ClockTime or CalendarTime.  Use SqlPOSIXTime instead.
              | SqlTimeDiff Integer -- ^ DEPRECATED Representation of TimeDiff.  Use SqlDiffTime instead.
              | SqlNull         -- ^ NULL in SQL or Nothing in Haskell
     deriving (Show, Typeable)

instance Eq SqlValue where
    SqlString a == SqlString b = a == b
    SqlByteString a == SqlByteString b = a == b
    SqlWord32 a == SqlWord32 b = a == b
    SqlWord64 a == SqlWord64 b = a == b
    SqlInt32 a == SqlInt32 b = a == b
    SqlInt64 a == SqlInt64 b = a == b
    SqlInteger a == SqlInteger b = a == b
    SqlChar a == SqlChar b = a == b
    SqlBool a == SqlBool b = a == b
    SqlDouble a == SqlDouble b = a == b
    SqlRational a == SqlRational b = a == b
    SqlLocalTimeOfDay a == SqlLocalTimeOfDay b = a == b
    SqlZonedLocalTimeOfDay a b == SqlZonedLocalTimeOfDay c d = a == c && b == d
    SqlLocalTime a == SqlLocalTime b = a == b
    SqlLocalDate a == SqlLocalDate b = a == b
    SqlZonedTime a == SqlZonedTime b = zonedTimeToUTC a == zonedTimeToUTC b
    SqlUTCTime a == SqlUTCTime b = a == b
    SqlPOSIXTime a == SqlPOSIXTime b = a == b
    SqlDiffTime a == SqlDiffTime b = a == b
    SqlEpochTime a == SqlEpochTime b = a == b
    SqlTimeDiff a == SqlTimeDiff b = a == b
    SqlNull == SqlNull = True
    SqlNull == _ = False
    _ == SqlNull = False
    a == b = ((safeFromSql a)::ConvertResult String) == 
             ((safeFromSql b)::ConvertResult String)

deriving instance Typeable ST.ClockTime
deriving instance Typeable ST.TimeDiff

instance Convertible SqlValue SqlValue where
    safeConvert = return

instance Convertible String SqlValue where
    safeConvert = return . SqlString
instance Convertible SqlValue String where
    safeConvert (SqlString x) = return x
    safeConvert (SqlByteString x) = return . BUTF8.toString $ x
    safeConvert (SqlInt32 x) = return . show $ x
    safeConvert (SqlInt64 x) = return . show $ x
    safeConvert (SqlWord32 x) = return . show $ x
    safeConvert (SqlWord64 x) = return . show $ x
    safeConvert (SqlInteger x) = return . show $ x
    safeConvert (SqlChar x) = return [x]
    safeConvert (SqlBool x) = return . show $ x
    safeConvert (SqlDouble x) = return . show $ x
    safeConvert (SqlRational x) = return . show $ x
    safeConvert (SqlLocalDate x) = 
        return . formatTime defaultTimeLocale (iso8601DateFormat Nothing) $ x
    safeConvert (SqlLocalTimeOfDay x) = 
        return . formatTime defaultTimeLocale "%T%Q" $ x
    safeConvert (SqlZonedLocalTimeOfDay tod tz) = 
        return $ formatTime defaultTimeLocale "%T%Q " tod ++
                 formatTime defaultTimeLocale "%z" tz
    safeConvert (SqlLocalTime x) = 
        return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q")) $ x
    safeConvert (SqlZonedTime x) = 
        return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q %z")) $ x
    safeConvert (SqlUTCTime x) = 
        return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q")) $ x
    safeConvert (SqlDiffTime x) = return $ showFixed True fixedval
            where fixedval :: Pico
                  fixedval = fromRational . toRational $ x
    safeConvert (SqlPOSIXTime x) = return $ showFixed True fixedval
            where fixedval :: Pico
                  fixedval = fromRational . toRational $ x
    safeConvert (SqlEpochTime x) = return . show $ x
    safeConvert (SqlTimeDiff x) = return . show $ x
    safeConvert y@(SqlNull) = quickError y

instance Convertible TS.Text SqlValue where
    safeConvert = return . SqlString . TS.unpack

instance Convertible SqlValue TS.Text where
    safeConvert = fmap TS.pack . safeConvert

instance Convertible TL.Text SqlValue where
    safeConvert = return . SqlString . TL.unpack

instance Convertible SqlValue TL.Text where
    safeConvert = fmap TL.pack . safeConvert

#ifdef __HUGS__
instance Typeable B.ByteString where
    typeOf _ = mkTypeName "ByteString"
#endif

instance Convertible B.ByteString SqlValue where
    safeConvert = return . SqlByteString
instance Convertible SqlValue B.ByteString where
    safeConvert (SqlByteString x) = return x
    safeConvert y@(SqlNull) = quickError y
    safeConvert x = safeConvert x >>= return . BUTF8.fromString

instance Convertible BSL.ByteString SqlValue where
    safeConvert = return . SqlByteString . B.concat . BSL.toChunks
instance Convertible SqlValue BSL.ByteString where
    safeConvert x = do bs <- safeConvert x
                       return (BSL.fromChunks [bs])

instance Convertible Int SqlValue where
    safeConvert x = 
        do i <- ((safeConvert x)::ConvertResult Int64)
           return $ SqlInt64 i
instance Convertible SqlValue Int where
    safeConvert (SqlString x) = read' x
    safeConvert (SqlByteString x) = (read' . BUTF8.toString) x
    safeConvert (SqlInt32 x) = safeConvert x
    safeConvert (SqlInt64 x) = safeConvert x
    safeConvert (SqlWord32 x) = safeConvert x
    safeConvert (SqlWord64 x) = safeConvert x
    safeConvert (SqlInteger x) = safeConvert x
    safeConvert (SqlChar x) = safeConvert x
    safeConvert (SqlBool x) = return (if x then 1 else 0)
    safeConvert (SqlDouble x) = safeConvert x
    safeConvert (SqlRational x) = safeConvert x
    safeConvert y@(SqlLocalDate _) = viaInteger y fromIntegral
    safeConvert y@(SqlLocalTimeOfDay _) = viaInteger y fromIntegral 
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = viaInteger y fromIntegral 
    safeConvert y@(SqlZonedTime _) = viaInteger y fromIntegral
    safeConvert (SqlUTCTime x) = safeConvert x
    safeConvert (SqlDiffTime x) = safeConvert x
    safeConvert (SqlPOSIXTime x) = safeConvert x
    safeConvert (SqlEpochTime x) = safeConvert x
    safeConvert (SqlTimeDiff x) = safeConvert x
    safeConvert y@(SqlNull) = quickError y

instance Convertible Int32 SqlValue where
    safeConvert = return . SqlInt32
instance Convertible SqlValue Int32 where
    safeConvert (SqlString x) = read' x
    safeConvert (SqlByteString x) = (read' . BUTF8.toString) x
    safeConvert (SqlInt32 x) = return x
    safeConvert (SqlInt64 x) = safeConvert x
    safeConvert (SqlWord32 x) = safeConvert x
    safeConvert (SqlWord64 x) = safeConvert x
    safeConvert (SqlInteger x) = safeConvert x
    safeConvert (SqlChar x) = safeConvert x
    safeConvert (SqlBool x) = return (if x then 1 else 0)
    safeConvert (SqlDouble x) = safeConvert x
    safeConvert (SqlRational x) = safeConvert x
    safeConvert y@(SqlLocalDate _) = viaInteger y fromIntegral
    safeConvert y@(SqlLocalTimeOfDay _) = viaInteger y fromIntegral
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlZonedTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlUTCTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlDiffTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlPOSIXTime _) = viaInteger y fromIntegral
    safeConvert (SqlEpochTime x) = safeConvert x
    safeConvert (SqlTimeDiff x) = safeConvert x
    safeConvert y@(SqlNull) = quickError y

instance Convertible Int64 SqlValue where
    safeConvert = return . SqlInt64
instance Convertible SqlValue Int64 where
    safeConvert (SqlString x) = read' x
    safeConvert (SqlByteString x) = (read' . BUTF8.toString) x
    safeConvert (SqlInt32 x) = safeConvert x
    safeConvert (SqlInt64 x) = return x
    safeConvert (SqlWord32 x) = safeConvert x
    safeConvert (SqlWord64 x) = safeConvert x
    safeConvert (SqlInteger x) = safeConvert x
    safeConvert (SqlChar x) = safeConvert x
    safeConvert (SqlBool x) = return (if x then 1 else 0)
    safeConvert (SqlDouble x) = safeConvert x
    safeConvert (SqlRational x) = safeConvert x
    safeConvert y@(SqlLocalDate _) = viaInteger y fromIntegral
    safeConvert y@(SqlLocalTimeOfDay _) = viaInteger y fromIntegral
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlZonedTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlUTCTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlDiffTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlPOSIXTime _) = viaInteger y fromIntegral
    safeConvert (SqlEpochTime x) = safeConvert x
    safeConvert (SqlTimeDiff x) = safeConvert x
    safeConvert y@(SqlNull) = quickError y

instance Convertible Word32 SqlValue where
    safeConvert = return . SqlWord32
instance Convertible SqlValue Word32 where
    safeConvert (SqlString x) = read' x
    safeConvert (SqlByteString x) = (read' . BUTF8.toString) x
    safeConvert (SqlInt32 x) = safeConvert x
    safeConvert (SqlInt64 x) = safeConvert x
    safeConvert (SqlWord32 x) = return x
    safeConvert (SqlWord64 x) = safeConvert x
    safeConvert (SqlInteger x) = safeConvert x
    safeConvert (SqlChar x) = safeConvert x
    safeConvert (SqlBool x) = return (if x then 1 else 0)
    safeConvert (SqlDouble x) = safeConvert x
    safeConvert (SqlRational x) = safeConvert x
    safeConvert y@(SqlLocalDate _) = viaInteger y fromIntegral
    safeConvert y@(SqlLocalTimeOfDay _) = viaInteger y fromIntegral
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlZonedTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlUTCTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlDiffTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlPOSIXTime _) = viaInteger y fromIntegral
    safeConvert (SqlEpochTime x) = safeConvert x
    safeConvert (SqlTimeDiff x) = safeConvert x
    safeConvert y@(SqlNull) = quickError y

instance Convertible Word64 SqlValue where
    safeConvert = return . SqlWord64
instance Convertible SqlValue Word64 where
    safeConvert (SqlString x) = read' x
    safeConvert (SqlByteString x) = (read' . BUTF8.toString) x
    safeConvert (SqlInt32 x) = safeConvert x
    safeConvert (SqlInt64 x) = safeConvert x
    safeConvert (SqlWord32 x) = safeConvert x
    safeConvert (SqlWord64 x) = return x
    safeConvert (SqlInteger x) = safeConvert x
    safeConvert (SqlChar x) = safeConvert x
    safeConvert (SqlBool x) = return (if x then 1 else 0)
    safeConvert (SqlDouble x) = safeConvert x
    safeConvert (SqlRational x) = safeConvert x
    safeConvert y@(SqlLocalDate _) = viaInteger y fromIntegral
    safeConvert y@(SqlLocalTimeOfDay _) = viaInteger y fromIntegral
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlZonedTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlUTCTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlDiffTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlPOSIXTime _) = viaInteger y fromIntegral
    safeConvert (SqlEpochTime x) = safeConvert x
    safeConvert (SqlTimeDiff x) = safeConvert x
    safeConvert y@(SqlNull) = quickError y

instance Convertible Integer SqlValue where
    safeConvert = return . SqlInteger
instance Convertible SqlValue Integer where
    safeConvert (SqlString x) = read' x
    safeConvert (SqlByteString x) = (read' . BUTF8.toString) x
    safeConvert (SqlInt32 x) = safeConvert x
    safeConvert (SqlInt64 x) = safeConvert x
    safeConvert (SqlWord32 x) = safeConvert x
    safeConvert (SqlWord64 x) = safeConvert x
    safeConvert (SqlInteger x) = return x
    safeConvert (SqlChar x) = safeConvert x
    safeConvert (SqlBool x) = return (if x then 1 else 0)
    safeConvert (SqlDouble x) = safeConvert x
    safeConvert (SqlRational x) = safeConvert x
    safeConvert (SqlLocalDate x) = return . toModifiedJulianDay $ x
    safeConvert (SqlLocalTimeOfDay x) = 
        return . fromIntegral . fromEnum . timeOfDayToTime $ x
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert (SqlZonedTime x) = 
        return . truncate . utcTimeToPOSIXSeconds . zonedTimeToUTC $ x
    safeConvert (SqlUTCTime x) = safeConvert x
    safeConvert (SqlDiffTime x) = safeConvert x
    safeConvert (SqlPOSIXTime x) = safeConvert x
    safeConvert (SqlEpochTime x) = return x
    safeConvert (SqlTimeDiff x) = return x
    safeConvert y@(SqlNull) = quickError y

instance Convertible Bool SqlValue where
    safeConvert = return . SqlBool
instance Convertible SqlValue Bool where
    safeConvert y@(SqlString x) = 
        case map toUpper x of
          "TRUE" -> Right True
          "T" -> Right True
          "FALSE" -> Right False
          "F" -> Right False
          "0" -> Right False
          "1" -> Right True
          _ -> convError "Cannot parse given String as Bool" y
    safeConvert (SqlByteString x) = (safeConvert . SqlString . BUTF8.toString) x
    safeConvert (SqlInt32 x) = numToBool x
    safeConvert (SqlInt64 x) = numToBool x
    safeConvert (SqlWord32 x) = numToBool x
    safeConvert (SqlWord64 x) = numToBool x
    safeConvert (SqlInteger x) = numToBool x
    safeConvert (SqlChar x) = numToBool (ord x)
    safeConvert (SqlBool x) = return x
    safeConvert (SqlDouble x) = numToBool x
    safeConvert (SqlRational x) = numToBool x
    safeConvert y@(SqlLocalDate _) = quickError y
    safeConvert y@(SqlLocalTimeOfDay _) = quickError y
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert y@(SqlZonedTime _) = quickError y
    safeConvert y@(SqlUTCTime _) = quickError y
    safeConvert y@(SqlDiffTime _) = quickError y
    safeConvert y@(SqlPOSIXTime _) = quickError y
    safeConvert (SqlEpochTime x) = numToBool x
    safeConvert (SqlTimeDiff x) = numToBool x
    safeConvert y@(SqlNull) = quickError y

numToBool :: (Eq a, Num a) => a -> ConvertResult Bool
numToBool x = Right (x /= 0)

instance Convertible Char SqlValue where
    safeConvert = return . SqlChar
instance Convertible SqlValue Char where
    safeConvert (SqlString [x]) = return x
    safeConvert y@(SqlString _) = convError "String length /= 1" y
    safeConvert (SqlByteString x) =
          safeConvert . SqlString . BUTF8.toString $ x
    safeConvert y@(SqlInt32 _) = quickError y
    safeConvert y@(SqlInt64 _) = quickError y
    safeConvert y@(SqlWord32 _) = quickError y
    safeConvert y@(SqlWord64 _) = quickError y
    safeConvert y@(SqlInteger _) = quickError y
    safeConvert (SqlChar x) = return x
    safeConvert (SqlBool x) = return (if x then '1' else '0')
    safeConvert y@(SqlDouble _) = quickError y
    safeConvert y@(SqlRational _) = quickError y
    safeConvert y@(SqlLocalDate _) = quickError y
    safeConvert y@(SqlLocalTimeOfDay _) = quickError y
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert y@(SqlZonedTime _) = quickError y
    safeConvert y@(SqlUTCTime _) = quickError y
    safeConvert y@(SqlDiffTime _) = quickError y
    safeConvert y@(SqlPOSIXTime _) = quickError y
    safeConvert y@(SqlEpochTime _) = quickError y
    safeConvert y@(SqlTimeDiff _) = quickError y
    safeConvert y@(SqlNull) = quickError y

instance Convertible Double SqlValue where
    safeConvert = return . SqlDouble
instance Convertible SqlValue Double where
    safeConvert (SqlString x) = read' x
    safeConvert (SqlByteString x) = (read' . BUTF8.toString) x
    safeConvert (SqlInt32 x) = safeConvert x
    safeConvert (SqlInt64 x) = safeConvert x
    safeConvert (SqlWord32 x) = safeConvert x
    safeConvert (SqlWord64 x) = safeConvert x
    safeConvert (SqlInteger x) = safeConvert x
    safeConvert (SqlChar x) = return . fromIntegral . fromEnum $ x
    safeConvert (SqlBool x) = return (if x then 1.0 else 0.0)
    safeConvert (SqlDouble x) = return x
    safeConvert (SqlRational x) = safeConvert x
    safeConvert y@(SqlLocalDate _) = ((safeConvert y)::ConvertResult Integer) >>= 
                                     (return . fromIntegral)
    safeConvert (SqlLocalTimeOfDay x) = 
        return . fromRational . toRational . timeOfDayToTime $ x
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert (SqlZonedTime x) = 
        safeConvert . SqlUTCTime . zonedTimeToUTC $ x
    safeConvert (SqlUTCTime x) = 
        return . fromRational . toRational . utcTimeToPOSIXSeconds $ x
    safeConvert (SqlDiffTime x) = safeConvert x
    safeConvert (SqlPOSIXTime x) = safeConvert x
    safeConvert (SqlEpochTime x) = safeConvert x
    safeConvert (SqlTimeDiff x) = safeConvert x
    safeConvert y@(SqlNull) = quickError y

instance Convertible Rational SqlValue where
    safeConvert = return . SqlRational
instance Convertible SqlValue Rational where
    safeConvert (SqlString x) = read' x
    safeConvert (SqlByteString x) = (read' . BUTF8.toString) x
    safeConvert (SqlInt32 x) = safeConvert x
    safeConvert (SqlInt64 x) = safeConvert x
    safeConvert (SqlWord32 x) = safeConvert x
    safeConvert (SqlWord64 x) = safeConvert x
    safeConvert (SqlInteger x) = safeConvert x
    safeConvert (SqlChar x) = return . fromIntegral . fromEnum $ x
    safeConvert (SqlBool x) = return $ if x then fromIntegral (1::Int) 
                                       else fromIntegral (0::Int)
    safeConvert (SqlDouble x) = safeConvert x
    safeConvert (SqlRational x) = return x
    safeConvert y@(SqlLocalDate _) = ((safeConvert y)::ConvertResult Integer) >>= 
                                     (return . fromIntegral)
    safeConvert (SqlLocalTimeOfDay x) = return . toRational . timeOfDayToTime $ x
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert (SqlZonedTime x) = safeConvert . SqlUTCTime . zonedTimeToUTC $ x
    safeConvert (SqlUTCTime x) = safeConvert x
    safeConvert (SqlDiffTime x) = safeConvert x
    safeConvert (SqlPOSIXTime x) = safeConvert x
    safeConvert (SqlEpochTime x) = return . fromIntegral $ x
    safeConvert (SqlTimeDiff x) = return . fromIntegral $ x
    safeConvert y@(SqlNull) = quickError y

instance Convertible Day SqlValue where
    safeConvert = return . SqlLocalDate
instance Convertible SqlValue Day where
    safeConvert (SqlString x) = parseTime' (iso8601DateFormat Nothing) x
    safeConvert (SqlByteString x) = safeConvert (SqlString (BUTF8.toString x))
    safeConvert (SqlInt32 x) = 
        return $ ModifiedJulianDay {toModifiedJulianDay = fromIntegral x}
    safeConvert (SqlInt64 x) = 
        return $ ModifiedJulianDay {toModifiedJulianDay = fromIntegral x}
    safeConvert (SqlWord32 x) = 
        return $ ModifiedJulianDay {toModifiedJulianDay = fromIntegral x}
    safeConvert (SqlWord64 x) = 
        return $ ModifiedJulianDay {toModifiedJulianDay = fromIntegral x}
    safeConvert (SqlInteger x) = 
        return $ ModifiedJulianDay {toModifiedJulianDay = x}
    safeConvert y@(SqlChar _) = quickError y
    safeConvert y@(SqlBool _) = quickError y
    safeConvert (SqlDouble x) = 
        return $ ModifiedJulianDay {toModifiedJulianDay = truncate x}
    safeConvert (SqlRational x) = safeConvert . SqlDouble . fromRational $ x
    safeConvert (SqlLocalDate x) = return x
    safeConvert y@(SqlLocalTimeOfDay _) = quickError y
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert (SqlLocalTime x) = return . localDay $ x
    safeConvert y@(SqlZonedTime _) = safeConvert y >>= return . localDay
    safeConvert y@(SqlUTCTime _) = safeConvert y >>= return . localDay
    safeConvert y@(SqlDiffTime _) = quickError y
    safeConvert y@(SqlPOSIXTime _) = safeConvert y >>= return . localDay
    safeConvert y@(SqlEpochTime _) = safeConvert y >>= return . localDay
    safeConvert y@(SqlTimeDiff _) = quickError y
    safeConvert y@(SqlNull) = quickError y

instance Convertible TimeOfDay SqlValue where
    safeConvert = return . SqlLocalTimeOfDay
instance Convertible SqlValue TimeOfDay where
    safeConvert (SqlString x) = parseTime' "%T%Q" x
    safeConvert (SqlByteString x) = safeConvert (SqlString (BUTF8.toString x))
    safeConvert (SqlInt32 x) = return . timeToTimeOfDay . fromIntegral $ x
    safeConvert (SqlInt64 x) = return . timeToTimeOfDay . fromIntegral $ x
    safeConvert (SqlWord32 x) = return . timeToTimeOfDay . fromIntegral $ x
    safeConvert (SqlWord64 x) = return . timeToTimeOfDay . fromIntegral $ x
    safeConvert (SqlInteger x) = return . timeToTimeOfDay . fromInteger $ x
    safeConvert y@(SqlChar _) = quickError y
    safeConvert y@(SqlBool _) = quickError y
    safeConvert (SqlDouble x) = 
        return . timeToTimeOfDay . fromIntegral $ ((truncate x)::Integer)
    safeConvert (SqlRational x) = safeConvert . SqlDouble . fromRational $ x
    safeConvert y@(SqlLocalDate _) = quickError y
    safeConvert (SqlLocalTimeOfDay x) = return x
    safeConvert (SqlZonedLocalTimeOfDay tod _) = return tod
    safeConvert (SqlLocalTime x) = return . localTimeOfDay $ x
    safeConvert y@(SqlZonedTime _) = safeConvert y >>= return . localTimeOfDay
    safeConvert y@(SqlUTCTime _) = safeConvert y >>= return . localTimeOfDay
    safeConvert y@(SqlDiffTime _) = quickError y
    safeConvert y@(SqlPOSIXTime _) = safeConvert y >>= return . localTimeOfDay
    safeConvert y@(SqlEpochTime _) = safeConvert y >>= return . localTimeOfDay
    safeConvert y@(SqlTimeDiff _) = quickError y
    safeConvert y@SqlNull = quickError y

instance Convertible (TimeOfDay, TimeZone) SqlValue where
    safeConvert (tod, tz) = return (SqlZonedLocalTimeOfDay tod tz)
instance Convertible SqlValue (TimeOfDay, TimeZone) where
    safeConvert (SqlString x) = 
        do tod <- parseTime' "%T%Q %z" x
           tz <- case parseTime defaultTimeLocale "%T%Q %z" x of
                      Nothing -> convError "Couldn't extract timezone in" (SqlString x)
                      Just y -> Right y
           return (tod, tz)
    safeConvert (SqlByteString x) = safeConvert (SqlString (BUTF8.toString x))
    safeConvert y@(SqlInt32 _) = quickError y
    safeConvert y@(SqlInt64 _) = quickError y
    safeConvert y@(SqlWord32 _) = quickError y
    safeConvert y@(SqlWord64 _) = quickError y
    safeConvert y@(SqlInteger _) = quickError y
    safeConvert y@(SqlChar _) = quickError y
    safeConvert y@(SqlBool _) = quickError y
    safeConvert y@(SqlDouble _) = quickError y
    safeConvert y@(SqlRational _) = quickError y
    safeConvert y@(SqlLocalDate _) = quickError y
    safeConvert y@(SqlLocalTimeOfDay _) = quickError y
    safeConvert (SqlZonedLocalTimeOfDay x y) = return (x, y)
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert (SqlZonedTime x) = return (localTimeOfDay . zonedTimeToLocalTime $ x,
                                           zonedTimeZone x)
    safeConvert y@(SqlUTCTime _) = quickError y
    safeConvert y@(SqlDiffTime _) = quickError y
    safeConvert y@(SqlPOSIXTime _) = quickError y
    safeConvert y@(SqlEpochTime _) = quickError y
    safeConvert y@(SqlTimeDiff _) = quickError y
    safeConvert y@SqlNull = quickError y

instance Convertible LocalTime SqlValue where
    safeConvert = return . SqlLocalTime
instance Convertible SqlValue LocalTime where
    safeConvert (SqlString x) = parseTime' (iso8601DateFormat (Just "%T%Q")) x
    safeConvert (SqlByteString x) = safeConvert (SqlString (BUTF8.toString x))
    safeConvert y@(SqlInt32 _) = quickError y
    safeConvert y@(SqlInt64 _) = quickError y
    safeConvert y@(SqlWord32 _) = quickError y
    safeConvert y@(SqlWord64 _) = quickError y
    safeConvert y@(SqlInteger _) = quickError y
    safeConvert y@(SqlChar _) = quickError y
    safeConvert y@(SqlBool _) = quickError y
    safeConvert y@(SqlDouble _) = quickError y
    safeConvert y@(SqlRational _) = quickError y
    safeConvert y@(SqlLocalDate _) = quickError y
    safeConvert y@(SqlLocalTimeOfDay _) = quickError y
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert (SqlLocalTime x) = return x
    safeConvert (SqlZonedTime x) = return . zonedTimeToLocalTime $ x
    safeConvert y@(SqlUTCTime _) = safeConvert y >>= return . zonedTimeToLocalTime
    safeConvert y@(SqlDiffTime _) = quickError y
    safeConvert y@(SqlPOSIXTime _) = safeConvert y >>= return . zonedTimeToLocalTime
    safeConvert y@(SqlEpochTime _) = safeConvert y >>= return . zonedTimeToLocalTime
    safeConvert y@(SqlTimeDiff _) = quickError y
    safeConvert y@SqlNull = quickError y

instance Convertible ZonedTime SqlValue where
    safeConvert = return . SqlZonedTime
instance Convertible SqlValue ZonedTime where
    safeConvert (SqlString x) = parseTime' (iso8601DateFormat (Just "%T%Q %z")) x
    safeConvert (SqlByteString x) = safeConvert (SqlString (BUTF8.toString x))
    safeConvert (SqlInt32 x) = safeConvert (SqlInteger (fromIntegral x))
    safeConvert (SqlInt64 x) = safeConvert (SqlInteger (fromIntegral x))
    safeConvert (SqlWord32 x) = safeConvert (SqlInteger (fromIntegral x))
    safeConvert (SqlWord64 x) = safeConvert (SqlInteger (fromIntegral x))
    safeConvert y@(SqlInteger _) = safeConvert y >>= return . utcToZonedTime utc
    safeConvert y@(SqlChar _) = quickError y
    safeConvert y@(SqlBool _) = quickError y
    safeConvert y@(SqlDouble _) = safeConvert y >>= return . utcToZonedTime utc
    safeConvert y@(SqlRational _) = safeConvert y >>= return . utcToZonedTime utc
    safeConvert y@(SqlLocalDate _) = quickError y
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert y@(SqlLocalTimeOfDay _) = quickError y
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert (SqlZonedTime x) = return x
    safeConvert (SqlUTCTime x) = return . utcToZonedTime utc $ x
    safeConvert y@(SqlDiffTime _) = quickError y
    safeConvert y@(SqlPOSIXTime _) = safeConvert y >>= return . utcToZonedTime utc
    safeConvert y@(SqlEpochTime _) = safeConvert y >>= return . utcToZonedTime utc
    safeConvert y@(SqlTimeDiff _) = quickError y
    safeConvert y@SqlNull = quickError y

instance Convertible UTCTime SqlValue where
    safeConvert = return . SqlUTCTime
instance Convertible SqlValue UTCTime where
    safeConvert (SqlString x) = parseTime' (iso8601DateFormat (Just "%T%Q")) x
    safeConvert (SqlByteString x) = safeConvert (SqlString (BUTF8.toString x))
    safeConvert y@(SqlInt32 _) = safeConvert y >>= return . posixSecondsToUTCTime
    safeConvert y@(SqlInt64 _) = safeConvert y >>= return . posixSecondsToUTCTime
    safeConvert y@(SqlWord32 _) = safeConvert y >>= return . posixSecondsToUTCTime
    safeConvert y@(SqlWord64 _) = safeConvert y >>= return . posixSecondsToUTCTime
    safeConvert y@(SqlInteger _) = safeConvert y >>= return . posixSecondsToUTCTime
    safeConvert y@(SqlChar _) = quickError y
    safeConvert y@(SqlBool _) = quickError y
    safeConvert y@(SqlDouble _) = safeConvert y >>= return . posixSecondsToUTCTime
    safeConvert y@(SqlRational _) = safeConvert y >>= return . posixSecondsToUTCTime
    safeConvert y@(SqlLocalDate _) = quickError y
    safeConvert y@(SqlLocalTimeOfDay _) = quickError y
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert (SqlZonedTime x) = return . zonedTimeToUTC $ x
    safeConvert (SqlUTCTime x) = return x
    safeConvert y@(SqlDiffTime _) = convError "incompatible types (did you mean SqlPOSIXTime?)" y
    safeConvert (SqlPOSIXTime x) = return . posixSecondsToUTCTime $ x
    safeConvert y@(SqlEpochTime _) = safeConvert y >>= return . posixSecondsToUTCTime
    safeConvert y@(SqlTimeDiff _) = convError "incompatible types (did you mean SqlPOSIXTime?)" y
    safeConvert y@SqlNull = quickError y

stringToPico :: String -> ConvertResult Pico
stringToPico s =
    let (base, fracwithdot) = span (/= '.') s
        shortfrac = drop 1 fracwithdot -- strip of dot; don't use tail because it may be empty
        frac = take 12 (rpad 12 '0' shortfrac)
        rpad :: Int -> a -> [a] -> [a]
                -- next line lifted from Data.Time
        rpad n c xs = xs ++ replicate (n - length xs) c
        mkPico :: Integer -> Integer -> Pico
                -- next line also lifted from Data.Time
        mkPico i f = fromInteger i + fromRational (f % 1000000000000)
    in do parsedBase <- read' base
          parsedFrac <- read' frac
          return (mkPico parsedBase parsedFrac)

instance Convertible NominalDiffTime SqlValue where
    safeConvert = return . SqlDiffTime
instance Convertible SqlValue NominalDiffTime where
    safeConvert (SqlString x) = stringToPico x >>= 
                                return . realToFrac
    safeConvert (SqlByteString x) = (stringToPico (BUTF8.toString x)) >>=
                                    return . realToFrac
    safeConvert (SqlInt32 x) = return . fromIntegral $ x
    safeConvert (SqlInt64 x) = return . fromIntegral $ x
    safeConvert (SqlWord32 x) = return . fromIntegral $ x
    safeConvert (SqlWord64 x) = return . fromIntegral $ x
    safeConvert (SqlInteger x) = return . fromIntegral $ x
    safeConvert y@(SqlChar _) = quickError y
    safeConvert y@(SqlBool _) = quickError y
    safeConvert (SqlDouble x) = return . fromRational . toRational $ x
    safeConvert (SqlRational x) = return . fromRational $ x
    safeConvert (SqlLocalDate x) = return . fromIntegral . (\y -> y * 60 * 60 * 24) . 
                               toModifiedJulianDay $ x
    safeConvert (SqlLocalTimeOfDay x) = 
        return . fromRational . toRational . timeOfDayToTime $ x
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert (SqlZonedTime x) = return . utcTimeToPOSIXSeconds . zonedTimeToUTC $ x
    safeConvert (SqlUTCTime x) = return . utcTimeToPOSIXSeconds $ x
    safeConvert (SqlDiffTime x) = return x
    safeConvert (SqlPOSIXTime x) = return x
    safeConvert (SqlEpochTime x) = return . fromIntegral $ x
    safeConvert (SqlTimeDiff x) = return . fromIntegral $ x
    safeConvert y@SqlNull = quickError y

instance Convertible ST.ClockTime SqlValue where
    safeConvert x = safeConvert x >>= return . SqlPOSIXTime
instance Convertible SqlValue ST.ClockTime where
    safeConvert (SqlString x) = do r <- read' x
                                   return $ ST.TOD r 0
    safeConvert (SqlByteString x) = safeConvert . SqlString . BUTF8.toString $ x
    safeConvert (SqlInt32 x) = return $ ST.TOD (fromIntegral x) 0
    safeConvert (SqlInt64 x) = return $ ST.TOD (fromIntegral x) 0
    safeConvert (SqlWord32 x) = return $ ST.TOD (fromIntegral x) 0
    safeConvert (SqlWord64 x) = return $ ST.TOD (fromIntegral x) 0
    safeConvert (SqlInteger x) = return $ ST.TOD x 0
    safeConvert y@(SqlChar _) = quickError y
    safeConvert y@(SqlBool _) = quickError y
    safeConvert (SqlDouble x) = return $ ST.TOD (truncate x) 0
    safeConvert (SqlRational x) = return $ ST.TOD (truncate x) 0
    safeConvert y@(SqlLocalDate _) = quickError y
    safeConvert y@(SqlLocalTimeOfDay _) = quickError y
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert y@(SqlZonedTime _) = safeConvert y >>= (\z -> return $ ST.TOD z 0)
    safeConvert y@(SqlUTCTime _) = safeConvert y >>= (\z -> return $ ST.TOD z 0)
    safeConvert y@(SqlDiffTime _) = quickError y
    safeConvert y@(SqlPOSIXTime _) = safeConvert y >>= (\z -> return $ ST.TOD z 0)
    safeConvert (SqlEpochTime x) = return $ ST.TOD x 0
    safeConvert y@(SqlTimeDiff _) = quickError y
    safeConvert y@SqlNull = quickError y

instance Convertible ST.TimeDiff SqlValue where
    safeConvert x = safeConvert x >>= return . SqlDiffTime
instance Convertible SqlValue ST.TimeDiff where
    safeConvert y@(SqlString _) = 
        do r <- safeConvert y
           safeConvert (SqlDiffTime r)
    safeConvert (SqlByteString x) = safeConvert . SqlString . BUTF8.toString $ x
    safeConvert (SqlInt32 x) = secs2td (fromIntegral x)
    safeConvert (SqlInt64 x) = secs2td (fromIntegral x)
    safeConvert (SqlWord32 x) = secs2td (fromIntegral x)
    safeConvert (SqlWord64 x) = secs2td (fromIntegral x)
    safeConvert (SqlInteger x) = secs2td x
    safeConvert y@(SqlChar _) = quickError y
    safeConvert y@(SqlBool _) = quickError y
    safeConvert (SqlDouble x) = secs2td (truncate x)
    safeConvert (SqlRational x) = secs2td (truncate x)
    safeConvert y@(SqlLocalDate _) = quickError y
    safeConvert y@(SqlLocalTimeOfDay _) = quickError y
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert y@(SqlZonedTime _) = quickError y
    safeConvert y@(SqlUTCTime _) = quickError y
    safeConvert y@(SqlPOSIXTime _) = quickError y
    safeConvert (SqlDiffTime x) = safeConvert x
    safeConvert y@(SqlEpochTime _) = quickError y
    safeConvert (SqlTimeDiff x) = secs2td x
    safeConvert y@SqlNull = quickError y

instance Convertible DiffTime SqlValue where
    safeConvert = return . SqlDiffTime . fromRational . toRational
instance Convertible SqlValue DiffTime where
    safeConvert (SqlString x) = read' x >>= return . fromInteger
    safeConvert (SqlByteString x) = safeConvert . SqlString . BUTF8.toString $ x
    safeConvert (SqlInt32 x) = return . fromIntegral $ x
    safeConvert (SqlInt64 x) = return . fromIntegral $ x
    safeConvert (SqlWord32 x) = return . fromIntegral $ x
    safeConvert (SqlWord64 x) = return . fromIntegral $ x
    safeConvert (SqlInteger x) = return . fromIntegral $ x
    safeConvert y@(SqlChar _) = quickError y
    safeConvert y@(SqlBool _) = quickError y
    safeConvert (SqlDouble x) = return . fromRational . toRational $ x
    safeConvert (SqlRational x) = return . fromRational $ x
    safeConvert y@(SqlLocalDate _) = quickError y
    safeConvert y@(SqlLocalTimeOfDay _) = quickError y
    safeConvert y@(SqlZonedLocalTimeOfDay _ _) = quickError y
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert y@(SqlZonedTime _) = quickError y
    safeConvert y@(SqlUTCTime _) = quickError y
    safeConvert (SqlDiffTime x) = return . fromRational . toRational $ x
    safeConvert y@(SqlPOSIXTime _) = quickError y
    safeConvert y@(SqlEpochTime _) = quickError y
    safeConvert (SqlTimeDiff x) = return . fromIntegral $ x
    safeConvert y@SqlNull = quickError y

instance Convertible ST.CalendarTime SqlValue where
    -- convert via ZonedTime
    safeConvert x = safeConvert x >>= return . SqlZonedTime
instance Convertible SqlValue ST.CalendarTime where
    -- convert via ZonedTime
    safeConvert = convertVia (undefined::ZonedTime)

instance (Convertible a SqlValue) => Convertible (Maybe a) SqlValue where
    safeConvert Nothing = return SqlNull
    safeConvert (Just a) = safeConvert a
instance (Convertible SqlValue a) => Convertible SqlValue (Maybe a) where
    safeConvert SqlNull = return Nothing
    safeConvert a = safeConvert a >>= (return . Just)

viaInteger' :: (Convertible SqlValue a, Bounded a, Show a, Convertible a Integer,
               Typeable a) => SqlValue -> (Integer -> ConvertResult a) -> ConvertResult a
viaInteger' sv func = 
    do i <- ((safeConvert sv)::ConvertResult Integer)
       boundedConversion func i

viaInteger :: (Convertible SqlValue a, Bounded a, Show a, Convertible a Integer,
               Typeable a) => SqlValue -> (Integer -> a) -> ConvertResult a
viaInteger sv func = viaInteger' sv (return . func)

secs2td :: Integer -> ConvertResult ST.TimeDiff
secs2td x = safeConvert x


-- | Read a value from a string, and give an informative message
--   if it fails.
read' :: (Typeable a, Read a, Convertible SqlValue a) => String -> ConvertResult a
read' s = 
    case reads s of
      [(x,"")] -> Right x
      _ -> convError "Cannot read source value as dest type" (SqlString s)

#ifdef __HUGS__
parseTime' :: (Typeable t, Convertible SqlValue t) => String -> String -> ConvertResult t
parseTime' _ inpstr =
    convError "Hugs does not support time parsing" (SqlString inpstr)
#else
parseTime' :: (Typeable t, Convertible SqlValue t, ParseTime t) => String -> String -> ConvertResult t
parseTime' fmtstr inpstr = 
    case parseTime defaultTimeLocale fmtstr inpstr of
      Nothing -> convError ("Cannot parse using default format string " ++ show fmtstr)
                 (SqlString inpstr)
      Just x -> Right x
#endif

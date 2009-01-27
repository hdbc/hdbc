module Database.HDBC.SqlValue
    (
    safeFromSql, toSql, fromSql,
    -- SqlType(..), nToSql, iToSql, posixToSql,
    SqlValue(..)
    )

-- FIXME: need to convert some of these time things to not be using viaInteger???

where
import Data.Dynamic
import qualified Data.ByteString as B
import Data.Char(ord,toUpper)
import Data.Word
import Data.Int
import qualified System.Time as ST
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar.OrdinalDate(sundayStartWeek, toOrdinalDate)
import System.Locale
import Data.Ratio
import Control.Monad.Error
import Data.Convertible

quickError :: (Typeable a, Convertible SqlValue a) => SqlValue -> ConvertResult a
quickError sv = convError "incompatible types" sv
  
{- | Convert a value to an 'SqlValue'.  This function is simply
a restricted-type wrapper around 'convert'.  See extended nots on 'safeFromSql'. -}
toSql :: Convertible a SqlValue => a -> SqlValue
toSql = convert

{- | Conversions to and from 'SqlValue's and standard Haskell types.

Conversions are powerful; for instance, you can call 'fromSql' on a SqlInt32
and get a String or a Double out of it.  This class attempts to Do
The Right Thing whenever possible, and will raise an error when asked to
do something incorrect.  In particular, when converting to any type
except a Maybe, 'SqlNull' as the input will cause an error to be raised.

Here are some notes about conversion:

 * Fractions of a second are not preserved on time values

See also 'nToSql', 'iToSql', 'posixToSql', 'SqlValue'.

This function converts from an 'SqlValue' to a Haskell value.  Many people will use the simpler
   'fromSql' instead.  This function is simply a restricted-type wrapper around
   'safeConvert'. -}
safeFromSql :: Convertible SqlValue a => SqlValue -> ConvertResult a
safeFromSql = safeConvert

{- | Convert from an 'SqlValue' to a Haskell value.  Any problem is indicated by
   calling 'error'.  This function is simply a restricted-type wrapper around
   'convert'.  See extended notes on 'safeFromSql'. -}
fromSql :: Convertible SqlValue a => SqlValue -> a
fromSql = convert

{- | Converts any Integral type to a 'SqlValue' by using toInteger. -}
nToSql :: Integral a => a -> SqlValue
nToSql n = SqlInteger (toInteger n)

{-
{- | Convenience function for using numeric literals in your program. -}
iToSql :: Int -> SqlValue
iToSql = toSql
-}

{- | Convenience function for converting 'POSIXTime' to a 'SqlValue', because
'toSql' cannot do the correct thing in this instance. -}
posixToSql :: POSIXTime -> SqlValue
posixToSql x = SqlPOSIXTime x

{- | The main type for expressing Haskell values to SQL databases.

This type is used to marshall Haskell data to and from database APIs.
HDBC driver interfaces will do their best to use the most accurate and
efficient way to send a particular value to the database server.

Values read back from the server are put in the most appropriate 'SqlValue'
type.  'fromSql' can then be used to convert them into whatever type
is needed locally in Haskell.

Most people will use 'toSql' and 'fromSql' instead of manipulating
'SqlValue's directly.

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

Two SqlValues are considered to be equal if one of these hold.  The
first comparison that can be made is controlling; if none of these
comparisons can be made, then they are not equal:

 * Both are NULL

 * Both represent the same type and the encapsulated values are considered equal
   by applying (==) to them

 * The values of each, when converted to a string, are equal.

Note that a 'NominalDiffTime' or 'POSIXTime' is converted to 'SqlDiffTime' by
'toSql'.  HDBC cannot differentiate between 'NominalDiffTime' and 'POSIXTime'
since they are the same underlying type.  You must construct 'SqlPOSIXTime'
manually or via 'posixToSql', or use 'SqlUTCTime'.

'SqlEpochTime' and 'SqlTimeDiff' are no longer created automatically by any
'toSql' or 'fromSql' functions.  They may still be manually constructed, but are
expected to be removed in a future version.  Although these two constructures will
be removed, support for marshalling to and from the old System.Time data will be
maintained as long as System.Time is, simply using the newer data types for conversion.

Default string representations are given as comments below where such are non-obvious.
These are used for 'fromSql' when a 'String' is desired.  They are also defaults for
representing data to SQL backends, though individual backends may override them
when a different format is demanded by the underlying database.  Date and time formats
use ISO8601 date format, with HH:MM:SS added for time, and -HHMM added for timezone
offsets.

See also: the extended notes on 'safeFromSql'.
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
              | SqlLocalTime LocalTime      -- ^ Local YYYY-MM-DD HH:MM:SS (no timezone)
              | SqlZonedTime ZonedTime      -- ^ Local YYYY-MM-DD HH:MM:SS -HHMM.  Considered equal if both convert to the same UTC time.
              | SqlUTCTime UTCTime          -- ^ UTC YYYY-MM-DD HH:MM:SS
              | SqlDiffTime NominalDiffTime -- ^ Calendar diff between seconds.  Rendered as Integer when converted to String, but greater precision may be preserved for other types or to underlying database.
              | SqlPOSIXTime POSIXTime      -- ^ Time as seconds since midnight Jan 1 1970 UTC.  Integer rendering as for 'SqlDiffTime'.
              | SqlEpochTime Integer      -- ^ DEPRECATED Representation of ClockTime or CalendarTime.  Use SqlPOSIXTime instead.
              | SqlTimeDiff Integer -- ^ DEPRECATED Representation of TimeDiff.  Use SqlDiffTime instead.
              | SqlNull         -- ^ NULL in SQL or Nothing in Haskell
     deriving (Show)

instance Typeable SqlValue where
    typeOf _ = mkTypeName "SqlValue"

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

instance Convertible String SqlValue where
    safeConvert = return . SqlString
instance Convertible SqlValue String where
    safeConvert (SqlString x) = return x
    safeConvert (SqlByteString x) = return . byteString2String $ x
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
        return . formatTime defaultTimeLocale "%T" $ x
    safeConvert (SqlLocalTime x) = 
        return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T")) $ x
    safeConvert (SqlZonedTime x) = 
        return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T %z")) $ x
    safeConvert (SqlUTCTime x) = 
        return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T")) $ x
    safeConvert (SqlDiffTime x) = return $ show ((truncate x)::Integer)
    safeConvert (SqlPOSIXTime x) = return $ show ((truncate x)::Integer)
    safeConvert (SqlEpochTime x) = return . show $ x
    safeConvert (SqlTimeDiff x) = return . show $ x
    safeConvert y@(SqlNull) = quickError y

instance Convertible B.ByteString SqlValue where
    safeConvert = return . SqlByteString
instance Convertible SqlValue B.ByteString where
    safeConvert (SqlByteString x) = return x
    safeConvert y@(SqlNull) = quickError y
    safeConvert x = safeConvert x >>= return . string2ByteString

string2ByteString :: String -> B.ByteString
string2ByteString = B.pack . map (toEnum . fromEnum)

instance Convertible Int SqlValue where
    safeConvert x = 
        do i <- ((safeConvert x)::ConvertResult Int32)
           return $ SqlInt32 i
instance Convertible SqlValue Int where
    safeConvert (SqlString x) = read' x
    safeConvert (SqlByteString x) = (read' . byteString2String) x
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
    safeConvert y@(SqlLocalTime _) = viaInteger y fromIntegral 
    safeConvert y@(SqlZonedTime _) = viaInteger y fromIntegral
    safeConvert y@(SqlUTCTime x) = safeConvert x
    safeConvert (SqlDiffTime x) = safeConvert x
    safeConvert (SqlPOSIXTime x) = safeConvert x
    safeConvert (SqlEpochTime x) = safeConvert x
    safeConvert (SqlTimeDiff x) = safeConvert x
    safeConvert y@(SqlNull) = quickError y

instance Convertible Int32 SqlValue where
    safeConvert = return . SqlInt32
instance Convertible SqlValue Int32 where
    safeConvert (SqlString x) = read' x
    safeConvert (SqlByteString x) = (read' . byteString2String) x
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
    safeConvert (SqlByteString x) = (read' . byteString2String) x
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
    safeConvert (SqlByteString x) = (read' . byteString2String) x
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
    safeConvert (SqlByteString x) = (read' . byteString2String) x
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
    safeConvert (SqlByteString x) = (read' . byteString2String) x
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
                 -- FIXME: stop generating this manually?
          _ -> Left $ ConvertError (show y) "SqlValue" "Bool" "Cannot parse given String as Bool"
    safeConvert (SqlByteString x) = (safeConvert . SqlString . byteString2String) x
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
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert y@(SqlZonedTime _) = quickError y
    safeConvert y@(SqlUTCTime _) = quickError y
    safeConvert y@(SqlDiffTime _) = quickError y
    safeConvert y@(SqlPOSIXTime _) = quickError y
    safeConvert (SqlEpochTime x) = numToBool x
    safeConvert (SqlTimeDiff x) = numToBool x
    safeConvert y@(SqlNull) = quickError y

numToBool :: Num a => a -> ConvertResult Bool
numToBool x = Right (x /= 0)

instance Convertible Char SqlValue where
    safeConvert = return . SqlChar
instance Convertible SqlValue Char where
    safeConvert (SqlString [x]) = return x
    safeConvert y@(SqlString _) = convError "String length /= 1" y
    safeConvert y@(SqlByteString x) = 
        case B.length x of
          1 -> safeConvert . SqlString . byteString2String $ x
          _ -> convError "ByteString length /= 1" y
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
    safeConvert (SqlByteString x) = (read' . byteString2String) x
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
    safeConvert (SqlByteString x) = (read' . byteString2String) x
    safeConvert (SqlInt32 x) = safeConvert x
    safeConvert (SqlInt64 x) = safeConvert x
    safeConvert (SqlWord32 x) = safeConvert x
    safeConvert (SqlWord64 x) = safeConvert x
    safeConvert (SqlInteger x) = safeConvert x
    safeConvert (SqlChar x) = return . fromIntegral . fromEnum $ x
    safeConvert (SqlBool x) = return $ if True then fromIntegral 1 else fromIntegral 0
    safeConvert (SqlDouble x) = safeConvert x
    safeConvert (SqlRational x) = return x
    safeConvert y@(SqlLocalDate _) = ((safeConvert y)::ConvertResult Integer) >>= 
                                     (return . fromIntegral)
    safeConvert (SqlLocalTimeOfDay x) = return . toRational . timeOfDayToTime $ x
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert (SqlZonedTime x) = safeConvert . SqlUTCTime . zonedTimeToUTC $ x
    safeConvert (SqlUTCTime x) = safeConvert x
    safeConvert (SqlDiffTime x) = safeConvert x
    safeConvert (SqlPOSIXTime x) = safeConvert x
    safeConvert (SqlEpochTime x) = return . fromIntegral $ x
    safeConvert (SqlTimeDiff x) = return . fromIntegral $ x
    safeConvert y@(SqlNull) = quickError y

instance Typeable Day where
    typeOf _ = mkTypeName "Day"
instance Typeable TimeOfDay where
    typeOf _ = mkTypeName "TimeOfDay"
instance Typeable LocalTime where
    typeOf _ = mkTypeName "LocalTime"
instance Typeable ZonedTime where
    typeOf _ = mkTypeName "ZonedTime"
instance Typeable ST.ClockTime where
    typeOf _ = mkTypeName "ClockTime"

instance Convertible Day SqlValue where
    safeConvert = return . SqlLocalDate
instance Convertible SqlValue Day where
    safeConvert (SqlString x) = parseTime' "Day" (iso8601DateFormat Nothing) x
    safeConvert (SqlByteString x) = safeConvert (SqlString (byteString2String x))
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
    safeConvert (SqlString x) = parseTime' "TimeOfDay" "%T" x
    safeConvert (SqlByteString x) = safeConvert (SqlString (byteString2String x))
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
    safeConvert (SqlLocalTime x) = return . localTimeOfDay $ x
    safeConvert y@(SqlZonedTime _) = safeConvert y >>= return . localTimeOfDay
    safeConvert y@(SqlUTCTime _) = safeConvert y >>= return . localTimeOfDay
    safeConvert y@(SqlDiffTime _) = quickError y
    safeConvert y@(SqlPOSIXTime _) = safeConvert y >>= return . localTimeOfDay
    safeConvert y@(SqlEpochTime _) = safeConvert y >>= return . localTimeOfDay
    safeConvert y@(SqlTimeDiff _) = quickError y
    safeConvert y@SqlNull = quickError y

instance Convertible LocalTime SqlValue where
    safeConvert = return . SqlLocalTime
instance Convertible SqlValue LocalTime where
    safeConvert (SqlString x) = parseTime' "LocalTime" (iso8601DateFormat (Just "%T")) x
    safeConvert (SqlByteString x) = safeConvert (SqlString (byteString2String x))
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
    safeConvert (SqlString x) = parseTime' "ZonedTime" (iso8601DateFormat (Just "%T %z")) x
    safeConvert (SqlByteString x) = safeConvert (SqlString (byteString2String x))
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
    safeConvert (SqlString x) = parseTime' "UTCTime" (iso8601DateFormat (Just "%T")) x
    safeConvert (SqlByteString x) = safeConvert (SqlString (byteString2String x))
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
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert (SqlZonedTime x) = return . zonedTimeToUTC $ x
    safeConvert (SqlUTCTime x) = return x
    safeConvert y@(SqlDiffTime _) = convError "incompatible types (did you mean SqlPOSIXTime?)" y
    safeConvert (SqlPOSIXTime x) = return . posixSecondsToUTCTime $ x
    safeConvert y@(SqlEpochTime _) = safeConvert y >>= return . posixSecondsToUTCTime
    safeConvert y@(SqlTimeDiff _) = convError "incompatible types (did you mean SqlPOSIXTime?)" y
    safeConvert y@SqlNull = quickError y

instance Convertible NominalDiffTime SqlValue where
    safeConvert = return . SqlDiffTime
instance Convertible SqlValue NominalDiffTime where
    safeConvert (SqlString x) = read' x >>= return . fromInteger
    safeConvert (SqlByteString x) = read' (byteString2String x) >>= return . fromInteger
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
    safeConvert (SqlByteString x) = safeConvert . SqlString . byteString2String $ x
    safeConvert y@(SqlInt32 x) = return $ ST.TOD (fromIntegral x) 0
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
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert y@(SqlZonedTime _) = safeConvert y >>= (\z -> return $ ST.TOD z 0)
    safeConvert y@(SqlUTCTime _) = safeConvert y >>= (\z -> return $ ST.TOD z 0)
    safeConvert y@(SqlDiffTime _) = quickError y
    safeConvert y@(SqlPOSIXTime _) = safeConvert y >>= (\z -> return $ ST.TOD z 0)
    safeConvert (SqlEpochTime x) = return $ ST.TOD x 0
    safeConvert y@(SqlTimeDiff _) = quickError y
    safeConvert y@SqlNull = quickError y
{-
instance SqlType ST.TimeDiff where
    toSql x = SqlDiffTime . fromIntegral . timeDiffToSecs $ x
    safeConvert (SqlString x) = read' x >>= secs2td
    safeConvert (SqlByteString x) = safeConvert . SqlString . byteString2String $ x
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
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert y@(SqlZonedTime _) = quickError y
    safeConvert y@(SqlUTCTime _) = quickError y
    safeConvert y@(SqlPOSIXTime _) = quickError y
    safeConvert (SqlDiffTime x) = secs2td (truncate x)
    safeConvert y@(SqlEpochTime _) = quickError y
    safeConvert (SqlTimeDiff x) = secs2td x
    safeConvert y@SqlNull = quickError y

instance SqlType DiffTime where
    toSql x = SqlDiffTime . fromRational . toRational $ x
    safeConvert (SqlString x) = read' x >>= return . fromInteger
    safeConvert (SqlByteString x) = safeConvert . SqlString . byteString2String $ x
    safeConvert (SqlInt32 x) = return . fromIntegral $ x
    safeConvert (SqlInt64 x) = return . fromIntegral $ x
    safeConvert (SqlWord32 x) = return . fromIntegral $ x
    safeConvert (SqlWord64 x) = return . fromIntegral $ x
    safeConvert (SqlInteger x) = return . fromIntegral $ x
    safeConvert y@(SqlChar _) = quickError y
    safeConvert y@(SqlBool _) = quickError y
    safeConvert (SqlDouble x) = return $ fromIntegral ((truncate x)::Integer)
    safeConvert (SqlRational x) = return $ fromIntegral ((truncate x)::Integer)
    safeConvert y@(SqlLocalDate _) = quickError y
    safeConvert y@(SqlLocalTimeOfDay _) = quickError y
    safeConvert y@(SqlLocalTime _) = quickError y
    safeConvert y@(SqlZonedTime _) = quickError y
    safeConvert y@(SqlUTCTime _) = quickError y
    safeConvert (SqlDiffTime x) = return . fromRational . toRational $ x
    safeConvert y@(SqlPOSIXTime _) = quickError y
    safeConvert y@(SqlEpochTime _) = quickError y
    safeConvert (SqlTimeDiff x) = return . fromIntegral $ x
    safeConvert y@SqlNull = quickError y

instance SqlType ST.CalendarTime where
    toSql x = toSql . calendarTimeToZonedTime $ x
    safeConvert y = safeConvert y >>= return . zonedTimeToCalendarTime

zonedTimeToCalendarTime :: ZonedTime -> ST.CalendarTime
zonedTimeToCalendarTime zt =
    ST.CalendarTime {
            ST.ctYear = fromIntegral year,
            ST.ctMonth = toEnum (month - 1),
            ST.ctDay = day,
            ST.ctHour = todHour ltod,
            ST.ctMin = todMin ltod,
            ST.ctSec = secs,
            ST.ctPicosec = truncate $ (((toRational (todSec ltod) - (toRational secs)) * 1000000000000)::Rational),
            ST.ctWDay = toEnum . snd . sundayStartWeek . localDay . zonedTimeToLocalTime $ zt,
            ST.ctYDay = snd . toOrdinalDate . localDay . zonedTimeToLocalTime $ zt,
            ST.ctTZName = timeZoneName . zonedTimeZone $ zt,
            ST.ctTZ = (timeZoneMinutes . zonedTimeZone $ zt) * 60,
            ST.ctIsDST = timeZoneSummerOnly . zonedTimeZone $ zt
          }
    where (year, month, day) = toGregorian . localDay . zonedTimeToLocalTime $ zt
          ltod = localTimeOfDay . zonedTimeToLocalTime $ zt
          secs = (truncate . todSec $ ltod)::Int

calendarTimeToZonedTime :: ST.CalendarTime -> ZonedTime
calendarTimeToZonedTime ct =
    ZonedTime {
     zonedTimeToLocalTime = LocalTime {
       localDay = fromGregorian (fromIntegral $ ST.ctYear ct) 
                  (1 + (fromEnum $ ST.ctMonth ct))
                  (ST.ctDay ct),
       localTimeOfDay = TimeOfDay {
         todHour = ST.ctHour ct,
         todMin = ST.ctMin ct,
         todSec = (fromIntegral $ ST.ctSec ct) + 
                  fromRational (ST.ctPicosec ct % 1000000000000)
                        }
                            },
     zonedTimeZone = TimeZone {
                       timeZoneMinutes = ST.ctTZ ct `div` 60,
                       timeZoneSummerOnly = ST.ctIsDST ct,
                       timeZoneName = ST.ctTZName ct}
}

instance (SqlType a) => SqlType (Maybe a) where
    sqlTypeName x = "Maybe " ++ sqlTypeName x
    toSql Nothing = SqlNull
    toSql (Just a) = toSql a
    safeConvert SqlNull = return Nothing
    safeConvert x = safeConvert x >>= return . Just

-}
byteString2String :: B.ByteString -> String
byteString2String = map (toEnum . fromEnum) . B.unpack

viaInteger' :: (Convertible SqlValue a, Bounded a, Show a, Convertible a Integer,
               Typeable a) => SqlValue -> (Integer -> ConvertResult a) -> ConvertResult a
viaInteger' sv func = 
    do i <- ((safeConvert sv)::ConvertResult Integer)
       boundedConversion func i

viaInteger :: (Convertible SqlValue a, Bounded a, Show a, Convertible a Integer,
               Typeable a) => SqlValue -> (Integer -> a) -> ConvertResult a
viaInteger sv func = viaInteger' sv (return . func)

secs2td :: Integer -> ConvertResult ST.TimeDiff
secs2td x = return $ ST.diffClockTimes (ST.TOD x 0) (ST.TOD 0 0)


-- FIXME: eliminate manually-crafted ConvertEror here?

-- | Read a value from a string, and give an informative message
--   if it fails.
read' :: (Typeable a, Read a, Convertible SqlValue a) => String -> ConvertResult a
read' s = if True then ret else Right fake
  where ret = case reads s of
                  [(x,"")] -> Right x
                  _ -> Left $ ConvertError {convSourceValue = show (SqlString s),
                                            convSourceType = "SqlValue",
                                            convDestType = t,
                                            convErrorMessage = "Cannot read source value as dest type"}
        fake = fromSql (SqlString "fake")
        t = show . typeOf $ fake

-- FIXME: eliminate manually-crafted ConvertEror here?

parseTime' :: (Convertible SqlValue t, ParseTime t) => String -> String -> String -> ConvertResult t
parseTime' t fmtstr inpstr = ret
    where ret = 
              case parseTime defaultTimeLocale fmtstr inpstr of
                Nothing -> Left $ ConvertError {convSourceValue = show (SqlString inpstr),
                                                convSourceType = "SqlValue",
                                                convDestType = t,
                                                convErrorMessage = "Cannot parse using default format string " ++ show fmtstr}
                Just x -> Right x

--------------
-- The following function copied from MissingH.Time.hs

{- | Converts the given timeDiff to the number of seconds it represents.

Uses the same algorithm as normalizeTimeDiff in GHC. -}
timeDiffToSecs :: ST.TimeDiff -> Integer
timeDiffToSecs td =
    (fromIntegral $ ST.tdSec td) +
    60 * ((fromIntegral $ ST.tdMin td) +
          60 * ((fromIntegral $ ST.tdHour td) +
                24 * ((fromIntegral $ ST.tdDay td) +
                      30 * ((fromIntegral $ ST.tdMonth td) +
                            365 * (fromIntegral $ ST.tdYear td)))))

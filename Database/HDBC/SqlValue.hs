module Database.HDBC.SqlValue
    (
    SqlType(..), nToSql, iToSql, posixToSql,
    fromSql,
    FromSqlResult,
    SqlValue(..),
    SqlValueError(..),
    sqlValueErrorPretty
    )

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
import System.Locale
import Data.Ratio
import Control.Monad.Error

data SqlValueError = SqlValueError {
      sqlSourceValue :: String,
      sqlDestType :: String,
      sqlValueErrorMsg :: String}
      deriving (Eq, Read, Show, Typeable)

instance Error SqlValueError where
    strMsg x = SqlValueError {sqlSourceValue = "(unknown)",
                              sqlDestType = "(unknown)",
                              sqlValueErrorMsg = x}

sqlValueErrorPretty :: SqlValueError -> String
sqlValueErrorPretty sv =
    "fromSql: could not convert " ++ sqlSourceValue sv ++ " to " ++
    sqlDestType sv ++ ": " ++ sqlValueErrorMsg sv

{- | The result of 'safeFromSql'. -}
type FromSqlResult a = Either SqlValueError a

quickErrorMsg :: SqlType a => SqlValue -> String -> FromSqlResult a
quickErrorMsg sv msg = if True then Left ret else Right fake
    where ret = SqlValueError {sqlSourceValue = show sv,
                               sqlDestType = t,
                               sqlValueErrorMsg = msg}
          fake = fromSql (SqlString "fake")
          t = sqlTypeName fake

quickError :: SqlType a => SqlValue -> FromSqlResult a
quickError sv = quickErrorMsg sv "incompatible types"
  
{- | Conversions to and from 'SqlValue's and standard Haskell types.

Conversions are powerful; for instance, you can call 'fromSql' on a SqlInt32
and get a String or a Double out of it.  This class attempts to Do
The Right Thing whenever possible, and will raise an error when asked to
do something incorrect.  In particular, when converting to any type
except a Maybe, 'SqlNull' as the input will cause an error to be raised.

Here are some notes about conversion:

 * Fractions of a second are not preserved on time values

See also 'nToSql', 'iToSql', 'posixToSql'.
-}
class (Show a) => SqlType a where
    {- | Convert a value to an 'SqlValue' -}
    toSql :: a -> SqlValue

    {- | Convert from an 'SqlValue' to a Haskell value.

         Most people use the simpler 'fromSql' instead. -}
    safeFromSql :: SqlValue -> FromSqlResult a

    {- | The name for this type, primarily for internal use in generating 'SqlValueError'
       in an automated fashion.  This function is used because not all types
       we deal with are instances of Data.Typeable. -}
    sqlTypeName :: a -> String

{- | Convert from an SqlValue to a Haskell value.  Any problem is indicated
by calling 'error', after pretty-printing the error from 'safeFromSql'. -}
fromSql :: (SqlType a) => SqlValue -> a
fromSql sv = 
    case safeFromSql sv of
      Left sve -> error (sqlValueErrorPretty sve)
      Right r -> r

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
    a == b = ((safeFromSql a)::FromSqlResult String) == 
             ((safeFromSql b)::FromSqlResult String)

instance SqlType String where
    sqlTypeName _ = "String"
    toSql = SqlString
    safeFromSql (SqlString x) = return x
    safeFromSql (SqlByteString x) = return . byteString2String $ x
    safeFromSql (SqlInt32 x) = return . show $ x
    safeFromSql (SqlInt64 x) = return . show $ x
    safeFromSql (SqlWord32 x) = return . show $ x
    safeFromSql (SqlWord64 x) = return . show $ x
    safeFromSql (SqlInteger x) = return . show $ x
    safeFromSql (SqlChar x) = return [x]
    safeFromSql (SqlBool x) = return . show $ x
    safeFromSql (SqlDouble x) = return . show $ x
    safeFromSql (SqlRational x) = return . show $ x
    safeFromSql (SqlLocalDate x) = 
        return . formatTime defaultTimeLocale (iso8601DateFormat Nothing) $ x
    safeFromSql (SqlLocalTimeOfDay x) = 
        return . formatTime defaultTimeLocale "%T" $ x
    safeFromSql (SqlLocalTime x) = 
        return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T")) $ x
    safeFromSql (SqlZonedTime x) = 
        return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T %z")) $ x
    safeFromSql (SqlUTCTime x) = 
        return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T")) $ x
    safeFromSql (SqlDiffTime x) = return $ show ((truncate x)::Integer)
    safeFromSql (SqlPOSIXTime x) = return $ show ((truncate x)::Integer)
    safeFromSql (SqlEpochTime x) = return . show $ x
    safeFromSql (SqlTimeDiff x) = return . show $ x
    safeFromSql y@(SqlNull) = quickError y

instance SqlType B.ByteString where
    sqlTypeName _ = "ByteString"
    toSql = SqlByteString
    safeFromSql (SqlByteString x) = return x
    safeFromSql y@(SqlNull) = quickError y
    safeFromSql x = safeFromSql x >>= return . string2ByteString

string2ByteString :: String -> B.ByteString
string2ByteString = B.pack . map (toEnum . fromEnum)

instance SqlType Int where
    sqlTypeName _ = "Int"
    toSql x = SqlInt32 (fromIntegral x)
    safeFromSql (SqlString x) = read' x
    safeFromSql (SqlByteString x) = (read' . byteString2String) x
    safeFromSql (SqlInt32 x) = return . fromIntegral $ x
    safeFromSql (SqlInt64 x) = return . fromIntegral $ x
    safeFromSql (SqlWord32 x) = return . fromIntegral $ x
    safeFromSql (SqlWord64 x) = return . fromIntegral $ x
    safeFromSql (SqlInteger x) = return . fromIntegral $ x
    safeFromSql (SqlChar x) = return . ord $ x
    safeFromSql (SqlBool x) = return (if x then 1 else 0)
    safeFromSql (SqlDouble x) = return . truncate $ x
    safeFromSql (SqlRational x) = return . truncate $ x
    safeFromSql y@(SqlLocalDate _) = viaInteger y fromIntegral
    safeFromSql y@(SqlLocalTimeOfDay _) = viaInteger y fromIntegral 
    safeFromSql y@(SqlLocalTime _) = viaInteger y fromIntegral 
    safeFromSql y@(SqlZonedTime _) = viaInteger y fromIntegral
    safeFromSql y@(SqlUTCTime _) = viaInteger y fromIntegral
    safeFromSql (SqlDiffTime x) = return . truncate $ x
    safeFromSql (SqlPOSIXTime x) = return . truncate $ x
    safeFromSql (SqlEpochTime x) = return . fromIntegral $ x
    safeFromSql (SqlTimeDiff x) = return . fromIntegral $ x
    safeFromSql y@(SqlNull) = quickError y

instance SqlType Int32 where
    sqlTypeName _ = "Int32"
    toSql = SqlInt32
    safeFromSql (SqlString x) = read' x
    safeFromSql (SqlByteString x) = (read' . byteString2String) x
    safeFromSql (SqlInt32 x) = return x
    safeFromSql (SqlInt64 x) = return . fromIntegral $ x
    safeFromSql (SqlWord32 x) = return . fromIntegral $ x
    safeFromSql (SqlWord64 x) = return . fromIntegral $ x
    safeFromSql (SqlInteger x) = return . fromIntegral $ x
    safeFromSql (SqlChar x) = return . fromIntegral . ord $ x
    safeFromSql (SqlBool x) = return (if x then 1 else 0)
    safeFromSql (SqlDouble x) = return . truncate $ x
    safeFromSql (SqlRational x) = return . truncate $ x
    safeFromSql y@(SqlLocalDate _) = viaInteger y fromIntegral
    safeFromSql y@(SqlLocalTimeOfDay _) = viaInteger y fromIntegral
    safeFromSql y@(SqlLocalTime _) = viaInteger y fromIntegral
    safeFromSql y@(SqlZonedTime _) = viaInteger y fromIntegral
    safeFromSql y@(SqlUTCTime _) = viaInteger y fromIntegral
    safeFromSql (SqlDiffTime x) = return . truncate $ x
    safeFromSql (SqlPOSIXTime x) = return . truncate $ x
    safeFromSql (SqlEpochTime x) = return . fromIntegral $ x
    safeFromSql (SqlTimeDiff x) = return . fromIntegral $ x
    safeFromSql y@(SqlNull) = quickError y

instance SqlType Int64 where
    sqlTypeName _ = "Int64"
    toSql = SqlInt64
    safeFromSql (SqlString x) = read' x
    safeFromSql (SqlByteString x) = (read' . byteString2String) x
    safeFromSql (SqlInt32 x) = return . fromIntegral $ x
    safeFromSql (SqlInt64 x) = return x
    safeFromSql (SqlWord32 x) = return . fromIntegral $ x
    safeFromSql (SqlWord64 x) = return . fromIntegral $ x
    safeFromSql (SqlInteger x) = return . fromIntegral $ x
    safeFromSql (SqlChar x) = return . fromIntegral . ord $ x
    safeFromSql (SqlBool x) = return (if x then 1 else 0)
    safeFromSql (SqlDouble x) = return . truncate $ x
    safeFromSql (SqlRational x) = return . truncate $ x
    safeFromSql y@(SqlLocalDate _) = viaInteger y fromIntegral
    safeFromSql y@(SqlLocalTimeOfDay _) = viaInteger y fromIntegral 
    safeFromSql y@(SqlLocalTime _) = viaInteger y fromIntegral 
    safeFromSql y@(SqlZonedTime _) = viaInteger y fromIntegral
    safeFromSql y@(SqlUTCTime _) = viaInteger y fromIntegral
    safeFromSql (SqlDiffTime x) = return . truncate $ x
    safeFromSql (SqlPOSIXTime x) = return . truncate $ x
    safeFromSql (SqlEpochTime x) = return . fromIntegral $ x
    safeFromSql (SqlTimeDiff x) = return . fromIntegral $ x
    safeFromSql y@(SqlNull) = quickError y

instance SqlType Word32 where
    sqlTypeName _ = "Word32"
    toSql = SqlWord32
    safeFromSql (SqlString x) = read' x
    safeFromSql (SqlByteString x) = (read' . byteString2String) x
    safeFromSql (SqlInt32 x) = return . fromIntegral $ x
    safeFromSql (SqlInt64 x) = return . fromIntegral $ x
    safeFromSql (SqlWord32 x) = return x
    safeFromSql (SqlWord64 x) = return . fromIntegral $ x
    safeFromSql (SqlInteger x) = return . fromIntegral $ x
    safeFromSql (SqlChar x) = return . fromIntegral . ord $ x
    safeFromSql (SqlBool x) = return (if x then 1 else 0)
    safeFromSql (SqlDouble x) = return . truncate $ x
    safeFromSql (SqlRational x) = return . truncate $ x
    safeFromSql y@(SqlLocalDate _) = viaInteger y fromIntegral
    safeFromSql y@(SqlLocalTimeOfDay _) = viaInteger y fromIntegral
    safeFromSql y@(SqlLocalTime _) = viaInteger y fromIntegral
    safeFromSql y@(SqlZonedTime _) = viaInteger y fromIntegral
    safeFromSql y@(SqlUTCTime _) = viaInteger y fromIntegral
    safeFromSql (SqlDiffTime x) = return . truncate $ x
    safeFromSql (SqlPOSIXTime x) = return . truncate $ x
    safeFromSql (SqlEpochTime x) = return . fromIntegral $ x
    safeFromSql (SqlTimeDiff x) = return . fromIntegral $ x
    safeFromSql y@(SqlNull) = quickError y

instance SqlType Word64 where
    sqlTypeName _ = "Word64"
    toSql = SqlWord64
    safeFromSql (SqlString x) = read' x
    safeFromSql (SqlByteString x) = (read' . byteString2String) x
    safeFromSql (SqlInt32 x) = return . fromIntegral $ x
    safeFromSql (SqlInt64 x) = return . fromIntegral $ x
    safeFromSql (SqlWord32 x) = return . fromIntegral $ x
    safeFromSql (SqlWord64 x) = return x
    safeFromSql (SqlInteger x) = return . fromIntegral $ x
    safeFromSql (SqlChar x) = return . fromIntegral . ord $ x
    safeFromSql (SqlBool x) = return (if x then 1 else 0)
    safeFromSql (SqlDouble x) = return . truncate $ x
    safeFromSql (SqlRational x) = return . truncate $ x
    safeFromSql y@(SqlLocalDate _) = viaInteger y fromIntegral
    safeFromSql y@(SqlLocalTimeOfDay _) = viaInteger y fromIntegral
    safeFromSql y@(SqlLocalTime _) = viaInteger y fromIntegral
    safeFromSql y@(SqlZonedTime _) = viaInteger y fromIntegral
    safeFromSql y@(SqlUTCTime _) = viaInteger y fromIntegral
    safeFromSql (SqlDiffTime x) = return . truncate $ x
    safeFromSql (SqlPOSIXTime x) = return . truncate $ x
    safeFromSql (SqlEpochTime x) = return . fromIntegral $ x
    safeFromSql (SqlTimeDiff x) = return . fromIntegral $ x
    safeFromSql y@(SqlNull) = quickError y

instance SqlType Integer where
    sqlTypeName _ = "Integer"
    toSql = SqlInteger
    safeFromSql (SqlString x) = read' x
    safeFromSql (SqlByteString x) = (read' . byteString2String) x
    safeFromSql (SqlInt32 x) = return . fromIntegral $ x
    safeFromSql (SqlInt64 x) = return . fromIntegral $ x
    safeFromSql (SqlWord32 x) = return . fromIntegral $ x
    safeFromSql (SqlWord64 x) = return . fromIntegral $ x
    safeFromSql (SqlInteger x) = return x
    safeFromSql (SqlChar x) = return . fromIntegral . ord $ x
    safeFromSql (SqlBool x) = return (if x then 1 else 0)
    safeFromSql (SqlDouble x) = return . truncate $ x
    safeFromSql (SqlRational x) = return . truncate $ x
    safeFromSql (SqlLocalDate x) = return . toModifiedJulianDay $ x
    safeFromSql (SqlLocalTimeOfDay x) = 
        return . fromIntegral . fromEnum . timeOfDayToTime $ x
    safeFromSql y@(SqlLocalTime _) = quickError y
    safeFromSql (SqlZonedTime x) = 
        return . truncate . utcTimeToPOSIXSeconds . zonedTimeToUTC $ x
    safeFromSql (SqlUTCTime x) =
        return . truncate . utcTimeToPOSIXSeconds $ x
    safeFromSql (SqlDiffTime x) = return . truncate $ x
    safeFromSql (SqlPOSIXTime x) = return . truncate $ x
    safeFromSql (SqlEpochTime x) = return x
    safeFromSql (SqlTimeDiff x) = return x
    safeFromSql y@(SqlNull) = quickError y

instance SqlType Bool where
    sqlTypeName _ = "Bool"
    toSql = SqlBool
    safeFromSql y@(SqlString x) = 
        case map toUpper x of
          "TRUE" -> Right True
          "T" -> Right True
          "FALSE" -> Right False
          "F" -> Right False
          "0" -> Right False
          "1" -> Right True
          _ -> Left $ SqlValueError (show y) "Bool" "Cannot parse given String as Bool"
    safeFromSql (SqlByteString x) = (safeFromSql . SqlString . byteString2String) x
    safeFromSql (SqlInt32 x) = numToBool x
    safeFromSql (SqlInt64 x) = numToBool x
    safeFromSql (SqlWord32 x) = numToBool x
    safeFromSql (SqlWord64 x) = numToBool x
    safeFromSql (SqlInteger x) = numToBool x
    safeFromSql (SqlChar x) = numToBool (ord x)
    safeFromSql (SqlBool x) = return x
    safeFromSql (SqlDouble x) = numToBool x
    safeFromSql (SqlRational x) = numToBool x
    safeFromSql y@(SqlLocalDate _) = quickError y
    safeFromSql y@(SqlLocalTimeOfDay _) = quickError y
    safeFromSql y@(SqlLocalTime _) = quickError y
    safeFromSql y@(SqlZonedTime _) = quickError y
    safeFromSql y@(SqlUTCTime _) = quickError y
    safeFromSql y@(SqlDiffTime _) = quickError y
    safeFromSql y@(SqlPOSIXTime _) = quickError y
    safeFromSql (SqlEpochTime x) = numToBool x
    safeFromSql (SqlTimeDiff x) = numToBool x
    safeFromSql y@(SqlNull) = quickError y

numToBool :: Num a => a -> FromSqlResult Bool
numToBool x = Right (x /= 0)

instance SqlType Char where
    sqlTypeName _ = "Char"
    toSql = SqlChar
    safeFromSql (SqlString [x]) = return x
    safeFromSql y@(SqlString _) = quickErrorMsg y "String length /= 1"
    safeFromSql y@(SqlByteString x) = 
        case B.length x of
          1 -> safeFromSql . SqlString . byteString2String $ x
          _ -> quickErrorMsg y "ByteString length /= 1"
    safeFromSql y@(SqlInt32 _) = quickError y
    safeFromSql y@(SqlInt64 _) = quickError y
    safeFromSql y@(SqlWord32 _) = quickError y
    safeFromSql y@(SqlWord64 _) = quickError y
    safeFromSql y@(SqlInteger _) = quickError y
    safeFromSql (SqlChar x) = return x
    safeFromSql (SqlBool x) = return (if x then '1' else '0')
    safeFromSql y@(SqlDouble _) = quickError y
    safeFromSql y@(SqlRational _) = quickError y
    safeFromSql y@(SqlLocalDate _) = quickError y
    safeFromSql y@(SqlLocalTimeOfDay _) = quickError y
    safeFromSql y@(SqlLocalTime _) = quickError y
    safeFromSql y@(SqlZonedTime _) = quickError y
    safeFromSql y@(SqlUTCTime _) = quickError y
    safeFromSql y@(SqlDiffTime _) = quickError y
    safeFromSql y@(SqlPOSIXTime _) = quickError y
    safeFromSql y@(SqlEpochTime _) = quickError y
    safeFromSql y@(SqlTimeDiff _) = quickError y
    safeFromSql y@(SqlNull) = quickError y

instance SqlType Double where
    sqlTypeName _ = "Double"
    toSql = SqlDouble
    safeFromSql (SqlString x) = read' x
    safeFromSql (SqlByteString x) = (read' . byteString2String) x
    safeFromSql (SqlInt32 x) = return . fromIntegral $ x
    safeFromSql (SqlInt64 x) = return . fromIntegral $ x
    safeFromSql (SqlWord32 x) = return . fromIntegral $ x
    safeFromSql (SqlWord64 x) = return . fromIntegral $ x
    safeFromSql (SqlInteger x) = return . fromIntegral $ x
    safeFromSql (SqlChar x) = return . fromIntegral . ord $ x
    safeFromSql (SqlBool x) = return (if x then 1.0 else 0.0)
    safeFromSql (SqlDouble x) = return x
    safeFromSql (SqlRational x) = return . fromRational $ x
    safeFromSql y@(SqlLocalDate _) = viaInteger y fromIntegral
    safeFromSql (SqlLocalTimeOfDay x) = 
        return . fromRational . toRational . timeOfDayToTime $ x
    safeFromSql y@(SqlLocalTime _) = quickError y
    safeFromSql (SqlZonedTime x) = safeFromSql . SqlUTCTime . zonedTimeToUTC $ x
    safeFromSql (SqlUTCTime x) = 
        return . fromRational . toRational . utcTimeToPOSIXSeconds $ x
    safeFromSql (SqlDiffTime x) = return . fromRational . toRational $ x
    safeFromSql (SqlPOSIXTime x) = return . fromRational . toRational $ x
    safeFromSql (SqlEpochTime x) = return . fromIntegral $ x
    safeFromSql (SqlTimeDiff x) = return . fromIntegral $ x
    safeFromSql y@(SqlNull) = quickError y

instance SqlType Rational where
    sqlTypeName _ = "Rational"
    toSql = SqlRational
    safeFromSql (SqlString x) = read' x
    safeFromSql (SqlByteString x) = (read' . byteString2String) x
    safeFromSql (SqlInt32 x) = return . fromIntegral $ x
    safeFromSql (SqlInt64 x) = return . fromIntegral $ x
    safeFromSql (SqlWord32 x) = return . fromIntegral $ x
    safeFromSql (SqlWord64 x) = return . fromIntegral $ x
    safeFromSql (SqlInteger x) = return . fromIntegral $ x
    safeFromSql (SqlChar x) = return . fromIntegral . ord $ x
    safeFromSql y@(SqlBool _) = viaInteger y fromIntegral
    safeFromSql (SqlDouble x) = return . toRational $ x
    safeFromSql (SqlRational x) = return x
    safeFromSql y@(SqlLocalDate _) = viaInteger y fromIntegral
    safeFromSql (SqlLocalTimeOfDay x) = return . toRational . timeOfDayToTime $ x
    safeFromSql y@(SqlLocalTime _) = quickError y
    safeFromSql (SqlZonedTime x) = safeFromSql . SqlUTCTime . zonedTimeToUTC $ x
    safeFromSql (SqlUTCTime x) = return . toRational . utcTimeToPOSIXSeconds $ x
    safeFromSql (SqlDiffTime x) = return . toRational $ x
    safeFromSql (SqlPOSIXTime x) = return . toRational $ x
    safeFromSql (SqlEpochTime x) = return . fromIntegral $ x
    safeFromSql (SqlTimeDiff x) = return . fromIntegral $ x
    safeFromSql y@(SqlNull) = quickError y

instance SqlType Day where
    sqlTypeName _ = "Day"
    toSql = SqlLocalDate
    safeFromSql (SqlString x) = parseTime' "Day" (iso8601DateFormat Nothing) x
    safeFromSql (SqlByteString x) = safeFromSql (SqlString (byteString2String x))
    safeFromSql (SqlInt32 x) = 
        return $ ModifiedJulianDay {toModifiedJulianDay = fromIntegral x}
    safeFromSql (SqlInt64 x) = 
        return $ ModifiedJulianDay {toModifiedJulianDay = fromIntegral x}
    safeFromSql (SqlWord32 x) = 
        return $ ModifiedJulianDay {toModifiedJulianDay = fromIntegral x}
    safeFromSql (SqlWord64 x) = 
        return $ ModifiedJulianDay {toModifiedJulianDay = fromIntegral x}
    safeFromSql (SqlInteger x) = 
        return $ ModifiedJulianDay {toModifiedJulianDay = x}
    safeFromSql y@(SqlChar _) = quickError y
    safeFromSql y@(SqlBool _) = quickError y
    safeFromSql (SqlDouble x) = 
        return $ ModifiedJulianDay {toModifiedJulianDay = truncate x}
    safeFromSql (SqlRational x) = safeFromSql . SqlDouble . fromRational $ x
    safeFromSql (SqlLocalDate x) = return x
    safeFromSql y@(SqlLocalTimeOfDay _) = quickError y
    safeFromSql (SqlLocalTime x) = return . localDay $ x
    safeFromSql y@(SqlZonedTime _) = safeFromSql y >>= return . localDay
    safeFromSql y@(SqlUTCTime _) = safeFromSql y >>= return . localDay
    safeFromSql y@(SqlDiffTime _) = quickError y
    safeFromSql y@(SqlPOSIXTime _) = safeFromSql y >>= return . localDay
    safeFromSql y@(SqlEpochTime _) = safeFromSql y >>= return . localDay
    safeFromSql y@(SqlTimeDiff _) = quickError y
    safeFromSql y@(SqlNull) = quickError y

instance SqlType TimeOfDay where
    sqlTypeName _ = "TimeOfDay"
    toSql = SqlLocalTimeOfDay
    safeFromSql (SqlString x) = parseTime' "TimeOfDay" "%T" x
    safeFromSql (SqlByteString x) = safeFromSql (SqlString (byteString2String x))
    safeFromSql (SqlInt32 x) = return . timeToTimeOfDay . fromIntegral $ x
    safeFromSql (SqlInt64 x) = return . timeToTimeOfDay . fromIntegral $ x
    safeFromSql (SqlWord32 x) = return . timeToTimeOfDay . fromIntegral $ x
    safeFromSql (SqlWord64 x) = return . timeToTimeOfDay . fromIntegral $ x
    safeFromSql (SqlInteger x) = return . timeToTimeOfDay . fromInteger $ x
    safeFromSql y@(SqlChar _) = quickError y
    safeFromSql y@(SqlBool _) = quickError y
    safeFromSql (SqlDouble x) = 
        return . timeToTimeOfDay . fromIntegral $ ((truncate x)::Integer)
    safeFromSql (SqlRational x) = safeFromSql . SqlDouble . fromRational $ x
    safeFromSql y@(SqlLocalDate _) = quickError y
    safeFromSql (SqlLocalTimeOfDay x) = return x
    safeFromSql (SqlLocalTime x) = return . localTimeOfDay $ x
    safeFromSql y@(SqlZonedTime _) = safeFromSql y >>= return . localTimeOfDay
    safeFromSql y@(SqlUTCTime _) = safeFromSql y >>= return . localTimeOfDay
    safeFromSql y@(SqlDiffTime _) = quickError y
    safeFromSql y@(SqlPOSIXTime _) = safeFromSql y >>= return . localTimeOfDay
    safeFromSql y@(SqlEpochTime _) = safeFromSql y >>= return . localTimeOfDay
    safeFromSql y@(SqlTimeDiff _) = quickError y
    safeFromSql y@SqlNull = quickError y

instance SqlType LocalTime where
    sqlTypeName _ = "LocalTime"
    toSql = SqlLocalTime
    safeFromSql (SqlString x) = parseTime' "LocalTime" (iso8601DateFormat (Just "%T")) x
    safeFromSql (SqlByteString x) = safeFromSql (SqlString (byteString2String x))
    safeFromSql y@(SqlInt32 _) = quickError y
    safeFromSql y@(SqlInt64 _) = quickError y
    safeFromSql y@(SqlWord32 _) = quickError y
    safeFromSql y@(SqlWord64 _) = quickError y
    safeFromSql y@(SqlInteger _) = quickError y
    safeFromSql y@(SqlChar _) = quickError y
    safeFromSql y@(SqlBool _) = quickError y
    safeFromSql y@(SqlDouble _) = quickError y
    safeFromSql y@(SqlRational _) = quickError y
    safeFromSql y@(SqlLocalDate _) = quickError y
    safeFromSql y@(SqlLocalTimeOfDay _) = quickError y
    safeFromSql (SqlLocalTime x) = return x
    safeFromSql (SqlZonedTime x) = return . zonedTimeToLocalTime $ x
    safeFromSql y@(SqlUTCTime _) = safeFromSql y >>= return . zonedTimeToLocalTime
    safeFromSql y@(SqlDiffTime _) = quickError y
    safeFromSql y@(SqlPOSIXTime _) = safeFromSql y >>= return . zonedTimeToLocalTime
    safeFromSql y@(SqlEpochTime _) = safeFromSql y >>= return . zonedTimeToLocalTime
    safeFromSql y@(SqlTimeDiff _) = quickError y
    safeFromSql y@SqlNull = quickError y

instance SqlType ZonedTime where
    sqlTypeName _ = "ZonedTime"
    toSql x = SqlZonedTime x
    safeFromSql (SqlString x) = parseTime' "ZonedTime" (iso8601DateFormat (Just "%T %z")) x
    safeFromSql (SqlByteString x) = safeFromSql (SqlString (byteString2String x))
    safeFromSql (SqlInt32 x) = safeFromSql (SqlInteger (fromIntegral x))
    safeFromSql (SqlInt64 x) = safeFromSql (SqlInteger (fromIntegral x))
    safeFromSql (SqlWord32 x) = safeFromSql (SqlInteger (fromIntegral x))
    safeFromSql (SqlWord64 x) = safeFromSql (SqlInteger (fromIntegral x))
    safeFromSql y@(SqlInteger _) = safeFromSql y >>= return . utcToZonedTime utc
    safeFromSql y@(SqlChar _) = quickError y
    safeFromSql y@(SqlBool _) = quickError y
    safeFromSql y@(SqlDouble _) = safeFromSql y >>= return . utcToZonedTime utc
    safeFromSql y@(SqlRational _) = safeFromSql y >>= return . utcToZonedTime utc
    safeFromSql y@(SqlLocalDate _) = quickError y
    safeFromSql y@(SqlLocalTime _) = quickError y
    safeFromSql y@(SqlLocalTimeOfDay _) = quickError y
    safeFromSql (SqlZonedTime x) = return x
    safeFromSql (SqlUTCTime x) = return . utcToZonedTime utc $ x
    safeFromSql y@(SqlDiffTime _) = quickError y
    safeFromSql y@(SqlPOSIXTime _) = safeFromSql y >>= return . utcToZonedTime utc
    safeFromSql y@(SqlEpochTime _) = safeFromSql y >>= return . utcToZonedTime utc
    safeFromSql y@(SqlTimeDiff _) = quickError y
    safeFromSql y@SqlNull = quickError y

instance SqlType UTCTime where
    sqlTypeName _ = "UTCTime"
    toSql = SqlUTCTime
    safeFromSql (SqlString x) = parseTime' "UTCTime" (iso8601DateFormat (Just "%T")) x
    safeFromSql (SqlByteString x) = safeFromSql (SqlString (byteString2String x))
    safeFromSql y@(SqlInt32 _) = safeFromSql y >>= return . posixSecondsToUTCTime
    safeFromSql y@(SqlInt64 _) = safeFromSql y >>= return . posixSecondsToUTCTime
    safeFromSql y@(SqlWord32 _) = safeFromSql y >>= return . posixSecondsToUTCTime
    safeFromSql y@(SqlWord64 _) = safeFromSql y >>= return . posixSecondsToUTCTime
    safeFromSql y@(SqlInteger _) = safeFromSql y >>= return . posixSecondsToUTCTime
    safeFromSql y@(SqlChar _) = quickError y
    safeFromSql y@(SqlBool _) = quickError y
    safeFromSql y@(SqlDouble _) = safeFromSql y >>= return . posixSecondsToUTCTime
    safeFromSql y@(SqlRational _) = safeFromSql y >>= return . posixSecondsToUTCTime
    safeFromSql y@(SqlLocalDate _) = quickError y
    safeFromSql y@(SqlLocalTimeOfDay _) = quickError y
    safeFromSql y@(SqlLocalTime _) = quickError y
    safeFromSql (SqlZonedTime x) = return . zonedTimeToUTC $ x
    safeFromSql (SqlUTCTime x) = return x
    safeFromSql y@(SqlDiffTime _) = quickErrorMsg y "incompatible types (did you mean SqlPOSIXTime?)"
    safeFromSql (SqlPOSIXTime x) = return . posixSecondsToUTCTime $ x
    safeFromSql y@(SqlEpochTime _) = safeFromSql y >>= return . posixSecondsToUTCTime
    safeFromSql y@(SqlTimeDiff _) = quickErrorMsg y "incompatible types (did you mean SqlPOSIXTime?)"
    safeFromSql y@SqlNull = quickError y

instance SqlType NominalDiffTime where
    sqlTypeName _ = "NominalDiffTime/POSIXTime"
    toSql = SqlDiffTime
    safeFromSql (SqlString x) = read' x >>= return . fromInteger
    safeFromSql (SqlByteString x) = read' (byteString2String x) >>= return . fromInteger
    safeFromSql (SqlInt32 x) = return . fromIntegral $ x
    safeFromSql (SqlInt64 x) = return . fromIntegral $ x
    safeFromSql (SqlWord32 x) = return . fromIntegral $ x
    safeFromSql (SqlWord64 x) = return . fromIntegral $ x
    safeFromSql (SqlInteger x) = return . fromIntegral $ x
    safeFromSql y@(SqlChar _) = quickError y
    safeFromSql y@(SqlBool _) = quickError y
    safeFromSql (SqlDouble x) = return . fromRational . toRational $ x
    safeFromSql (SqlRational x) = return . fromRational $ x
    safeFromSql (SqlLocalDate x) = return . fromIntegral . (\y -> y * 60 * 60 * 24) . 
                               toModifiedJulianDay $ x
    safeFromSql (SqlLocalTimeOfDay x) = 
        return . fromRational . toRational . timeOfDayToTime $ x
    safeFromSql y@(SqlLocalTime _) = quickError y
    safeFromSql (SqlZonedTime x) = return . utcTimeToPOSIXSeconds . zonedTimeToUTC $ x
    safeFromSql (SqlUTCTime x) = return . utcTimeToPOSIXSeconds $ x
    safeFromSql (SqlDiffTime x) = return x
    safeFromSql (SqlPOSIXTime x) = return x
    safeFromSql (SqlEpochTime x) = return . fromIntegral $ x
    safeFromSql (SqlTimeDiff x) = return . fromIntegral $ x
    safeFromSql y@SqlNull = quickError y

instance SqlType ST.ClockTime where
    sqlTypeName _ = "ClockTime"
    toSql (ST.TOD x y) = SqlPOSIXTime . fromRational $ 
                                        fromInteger x + fromRational (y % 1000000000000)
    safeFromSql (SqlString x) = do r <- read' x
                                   return $ ST.TOD r 0
    safeFromSql (SqlByteString x) = safeFromSql . SqlString . byteString2String $ x
    safeFromSql (SqlInt32 x) = return $ ST.TOD (fromIntegral x) 0
    safeFromSql (SqlInt64 x) = return $ ST.TOD (fromIntegral x) 0
    safeFromSql (SqlWord32 x) = return $ ST.TOD (fromIntegral x) 0
    safeFromSql (SqlWord64 x) = return $ ST.TOD (fromIntegral x) 0
    safeFromSql (SqlInteger x) = return $ ST.TOD x 0
    safeFromSql y@(SqlChar _) = quickError y
    safeFromSql y@(SqlBool _) = quickError y
    safeFromSql (SqlDouble x) = return $ ST.TOD (truncate x) 0
    safeFromSql (SqlRational x) = return $ ST.TOD (truncate x) 0
    safeFromSql y@(SqlLocalDate _) = quickError y
    safeFromSql y@(SqlLocalTimeOfDay _) = quickError y
    safeFromSql y@(SqlLocalTime _) = quickError y
    safeFromSql y@(SqlZonedTime _) = safeFromSql y >>= (\z -> return $ ST.TOD z 0)
    safeFromSql y@(SqlUTCTime _) = safeFromSql y >>= (\z -> return $ ST.TOD z 0)
    safeFromSql y@(SqlDiffTime _) = quickError y
    safeFromSql y@(SqlPOSIXTime _) = safeFromSql y >>= (\z -> return $ ST.TOD z 0)
    safeFromSql (SqlEpochTime x) = return $ ST.TOD x 0
    safeFromSql y@(SqlTimeDiff _) = quickError y
    safeFromSql y@SqlNull = quickError y

instance SqlType ST.TimeDiff where
    sqlTypeName _ = "TimeDiff"
    toSql x = SqlDiffTime . fromIntegral . timeDiffToSecs $ x
    safeFromSql (SqlString x) = read' x >>= secs2td
    safeFromSql (SqlByteString x) = safeFromSql . SqlString . byteString2String $ x
    safeFromSql (SqlInt32 x) = secs2td (fromIntegral x)
    safeFromSql (SqlInt64 x) = secs2td (fromIntegral x)
    safeFromSql (SqlWord32 x) = secs2td (fromIntegral x)
    safeFromSql (SqlWord64 x) = secs2td (fromIntegral x)
    safeFromSql (SqlInteger x) = secs2td x
    safeFromSql y@(SqlChar _) = quickError y
    safeFromSql y@(SqlBool _) = quickError y
    safeFromSql (SqlDouble x) = secs2td (truncate x)
    safeFromSql (SqlRational x) = secs2td (truncate x)
    safeFromSql y@(SqlLocalDate _) = quickError y
    safeFromSql y@(SqlLocalTimeOfDay _) = quickError y
    safeFromSql y@(SqlLocalTime _) = quickError y
    safeFromSql y@(SqlZonedTime _) = quickError y
    safeFromSql y@(SqlUTCTime _) = quickError y
    safeFromSql y@(SqlPOSIXTime _) = quickError y
    safeFromSql (SqlDiffTime x) = secs2td (truncate x)
    safeFromSql y@(SqlEpochTime _) = quickError y
    safeFromSql (SqlTimeDiff x) = secs2td x
    safeFromSql y@SqlNull = quickError y

instance SqlType DiffTime where
    sqlTypeName _ = "DiffTime"
    toSql x = SqlDiffTime . fromRational . toRational $ x
    safeFromSql (SqlString x) = read' x >>= return . fromInteger
    safeFromSql (SqlByteString x) = safeFromSql . SqlString . byteString2String $ x
    safeFromSql (SqlInt32 x) = return . fromIntegral $ x
    safeFromSql (SqlInt64 x) = return . fromIntegral $ x
    safeFromSql (SqlWord32 x) = return . fromIntegral $ x
    safeFromSql (SqlWord64 x) = return . fromIntegral $ x
    safeFromSql (SqlInteger x) = return . fromIntegral $ x
    safeFromSql y@(SqlChar _) = quickError y
    safeFromSql y@(SqlBool _) = quickError y
    safeFromSql (SqlDouble x) = return $ fromIntegral ((truncate x)::Integer)
    safeFromSql (SqlRational x) = return $ fromIntegral ((truncate x)::Integer)
    safeFromSql y@(SqlLocalDate _) = quickError y
    safeFromSql y@(SqlLocalTimeOfDay _) = quickError y
    safeFromSql y@(SqlLocalTime _) = quickError y
    safeFromSql y@(SqlZonedTime _) = quickError y
    safeFromSql y@(SqlUTCTime _) = quickError y
    safeFromSql (SqlDiffTime x) = return . fromRational . toRational $ x
    safeFromSql y@(SqlPOSIXTime _) = quickError y
    safeFromSql y@(SqlEpochTime _) = quickError y
    safeFromSql (SqlTimeDiff x) = return . fromIntegral $ x
    safeFromSql y@SqlNull = quickError y

instance SqlType ST.CalendarTime where
    sqlTypeName _ = "CalendarTime"
    toSql x = toSql (ST.toClockTime x)
    safeFromSql y = safeFromSql y >>= return . ST.toUTCTime

instance (SqlType a) => SqlType (Maybe a) where
    sqlTypeName x = "Maybe " ++ sqlTypeName x
    toSql Nothing = SqlNull
    toSql (Just a) = toSql a
    safeFromSql SqlNull = return Nothing
    safeFromSql x = safeFromSql x >>= return . Just

byteString2String :: B.ByteString -> String
byteString2String = map (toEnum . fromEnum) . B.unpack

viaInteger :: SqlType a => SqlValue -> (Integer -> a) -> FromSqlResult a
viaInteger sv func = 
    do i <- safeFromSql sv
       return (func i)

secs2td :: Integer -> FromSqlResult ST.TimeDiff
secs2td x = return $ ST.diffClockTimes (ST.TOD x 0) (ST.TOD 0 0)


-- | Read a value from a string, and give an informative message
--   if it fails.
read' :: (Read a, SqlType a) => String -> FromSqlResult a
read' s = if True then ret else Right fake
  where ret = case reads s of
                  [(x,"")] -> Right x
                  _ -> Left $ SqlValueError {sqlSourceValue = show (SqlString s),
                                             sqlDestType = t,
                                             sqlValueErrorMsg = "Cannot read source value as dest type"}
        fake = fromSql (SqlString "fake")
        t = sqlTypeName fake

parseTime' :: (SqlType t, ParseTime t) => String -> String -> String -> FromSqlResult t
parseTime' t fmtstr inpstr = ret
    where ret = 
              case parseTime defaultTimeLocale fmtstr inpstr of
                Nothing -> Left $ SqlValueError {sqlSourceValue = show (SqlString inpstr),
                                                 sqlDestType = t,
                                                 sqlValueErrorMsg = "Cannot parse using default format string " ++ show fmtstr}
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

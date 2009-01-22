module Database.HDBC.SqlValue
    (
    SqlType(..), nToSql, iToSql,
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

type FromSqlResult a = Either SqlValueError a

quickError :: SqlType a => SqlValue -> FromSqlResult a
quickError sv = if True then Left ret else Right fake
    where ret = SqlValueError {sqlSourceValue = show sv,
                               sqlDestType = t,
                               sqlValueErrorMsg = "incompatible types"}
          fake = fromSql (SqlString "fake")
          t = sqlTypeName fake
  
{- | Conversions to and from 'SqlValue's and standard Haskell types.

Conversions are powerful; for instance, you can call 'fromSql' on a SqlInt32
and get a String or a Double out of it.  This class attempts to Do
The Right Thing whenever possible, and will raise an error when asked to
do something incorrect.  In particular, when converting to any type
except a Maybe, 'SqlNull' as the input will cause an error to be raised.

Here are some notes about conversion:

 * Fractions of a second are not preserved on time values

See also 'nToSql', 'iToSql'.
-}
class (Show a) => SqlType a where
    {- | Convert a value to an 'SqlValue' -}
    toSql :: a -> SqlValue

    {- | Convert from an 'SqlValue' to a Haskell value.

         Most people use 'fromSql' instead. -}
    safeFromSql :: SqlValue -> FromSqlResult a

    {- | The name for this type, primarily for use in generating 'SqlValueError'
       in an automated fashion.  This function is used because not all types
       we deal with are instances of Data.Typeable. -}
    sqlTypeName :: a -> String

{- | Convert from an SqlValue to a Haskell value.  Any problem is indicated
by calling 'error'. -}
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
'toSQL'.  HDBC cannot differentiate between 'NominalDiffTime' and 'POSIXTime'
since they are the same underlying type.  You must construct 'SqlPOSIXTime'
manually, or use 'SqlUTCTime'.

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
              | SqlPOSIXTime POSIXTime      -- ^ Time as seconds since 1/1/1970 UTC.  Integer rendering as for 'SqlDiffTime'.
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

{-
instance SqlType Int32 where
    toSql = SqlInt32
    safeFromSql (SqlString x) = read' x
    safeFromSql (SqlByteString x) = (read' . byteString2String) x
    safeFromSql (SqlInt32 x) = x
    safeFromSql (SqlInt64 x) = fromIntegral x
    safeFromSql (SqlWord32 x) = fromIntegral x
    safeFromSql (SqlWord64 x) = fromIntegral x
    safeFromSql (SqlInteger x) = fromIntegral x
    safeFromSql (SqlChar x) = fromIntegral $ ord x
    safeFromSql (SqlBool x) = if x then 1 else 0
    safeFromSql (SqlDouble x) = truncate $ x
    safeFromSql (SqlRational x) = truncate $ x
    safeFromSql y@(SqlLocalDate _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlLocalTimeOfDay _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlLocalTime _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlZonedTime _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlUTCTime _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql (SqlDiffTime x) = truncate x
    safeFromSql (SqlPOSIXTime x) = truncate x
    safeFromSql (SqlEpochTime x) = fromIntegral x
    safeFromSql (SqlTimeDiff x) = fromIntegral x
    safeFromSql (SqlNull) = error "safeFromSql: cannot convert SqlNull to Int32"

instance SqlType Int64 where
    toSql = SqlInt64
    safeFromSql (SqlString x) = read' x
    safeFromSql (SqlByteString x) = (read' . byteString2String) x
    safeFromSql (SqlInt32 x) = fromIntegral x
    safeFromSql (SqlInt64 x) = x
    safeFromSql (SqlWord32 x) = fromIntegral x
    safeFromSql (SqlWord64 x) = fromIntegral x
    safeFromSql (SqlInteger x) = fromIntegral x
    safeFromSql (SqlChar x) = fromIntegral $ ord x
    safeFromSql (SqlBool x) = if x then 1 else 0
    safeFromSql (SqlDouble x) = truncate $ x
    safeFromSql (SqlRational x) = truncate $ x
    safeFromSql y@(SqlLocalDate _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlLocalTimeOfDay _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlLocalTime _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlZonedTime _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlUTCTime _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql (SqlDiffTime x) = truncate x
    safeFromSql (SqlPOSIXTime x) = truncate x
    safeFromSql (SqlEpochTime x) = fromIntegral x
    safeFromSql (SqlTimeDiff x) = fromIntegral x
    safeFromSql (SqlNull) = error "safeFromSql: cannot convert SqlNull to Int64"

instance SqlType Word32 where
    toSql = SqlWord32
    safeFromSql (SqlString x) = read' x
    safeFromSql (SqlByteString x) = (read' . byteString2String) x
    safeFromSql (SqlInt32 x) = fromIntegral x
    safeFromSql (SqlInt64 x) = fromIntegral x
    safeFromSql (SqlWord32 x) = x
    safeFromSql (SqlWord64 x) = fromIntegral x
    safeFromSql (SqlInteger x) = fromIntegral x
    safeFromSql (SqlChar x) = fromIntegral $ ord x
    safeFromSql (SqlBool x) = if x then 1 else 0
    safeFromSql (SqlDouble x) = truncate $ x
    safeFromSql (SqlRational x) = truncate $ x
    safeFromSql y@(SqlLocalDate _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlLocalTimeOfDay _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlLocalTime _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlZonedTime _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlUTCTime _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql (SqlDiffTime x) = truncate x
    safeFromSql (SqlPOSIXTime x) = truncate x
    safeFromSql (SqlEpochTime x) = fromIntegral x
    safeFromSql (SqlTimeDiff x) = fromIntegral x
    safeFromSql (SqlNull) = error "safeFromSql: cannot convert SqlNull to Word32"

instance SqlType Word64 where
    toSql = SqlWord64
    safeFromSql (SqlString x) = read' x
    safeFromSql (SqlByteString x) = (read' . byteString2String) x
    safeFromSql (SqlInt32 x) = fromIntegral x
    safeFromSql (SqlInt64 x) = fromIntegral x
    safeFromSql (SqlWord32 x) = fromIntegral x
    safeFromSql (SqlWord64 x) = x
    safeFromSql (SqlInteger x) = fromIntegral x
    safeFromSql (SqlChar x) = fromIntegral (ord x)
    safeFromSql (SqlBool x) = if x then 1 else 0
    safeFromSql (SqlDouble x) = truncate $ x
    safeFromSql (SqlRational x) = truncate $ x
    safeFromSql y@(SqlLocalDate _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlLocalTimeOfDay _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlLocalTime _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlZonedTime _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql y@(SqlUTCTime _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql (SqlDiffTime x) = truncate x
    safeFromSql (SqlPOSIXTime x) = truncate x
    safeFromSql (SqlEpochTime x) = fromIntegral x
    safeFromSql (SqlTimeDiff x) = fromIntegral x
    safeFromSql (SqlNull) = error "safeFromSql: cannot convert SqlNull to Int64"
-}
instance SqlType Integer where
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
{-
instance SqlType Bool where
    toSql = SqlBool
    safeFromSql (SqlString x) = 
        case map toUpper x of
                           "TRUE" -> True
                           "T" -> True
                           "FALSE" -> False
                           "F" -> False
                           "0" -> False
                           "1" -> True
                           _ -> error $ "safeFromSql: cannot convert SqlString " 
                                        ++ show x ++ " to Bool"
    safeFromSql (SqlByteString x) = (safeFromSql . SqlString . byteString2String) x
    safeFromSql (SqlInt32 x) = numToBool x
    safeFromSql (SqlInt64 x) = numToBool x
    safeFromSql (SqlWord32 x) = numToBool x
    safeFromSql (SqlWord64 x) = numToBool x
    safeFromSql (SqlInteger x) = numToBool x
    safeFromSql (SqlChar x) = numToBool (ord x)
    safeFromSql (SqlBool x) = x
    safeFromSql (SqlDouble x) = numToBool x
    safeFromSql (SqlRational x) = numToBool x
    safeFromSql (SqlLocalDate _) = error "safeFromSql: cannot convert SqlLocalDate to Bool"
    safeFromSql (SqlLocalTimeOfDay _) = error "safeFromSql: cannot convert SqlLocalTimeOfDay to Bool"
    safeFromSql (SqlLocalTime _) = error "safeFromSql: cannot convert SqlLocalTime to Bool"
    safeFromSql (SqlZonedTime _) = error "safeFromSql: cannot convert SqlZonedTime to Bool"
    safeFromSql (SqlUTCTime _) = error "safeFromSql: cannot convert SqlUTCTime to Bool"
    safeFromSql (SqlDiffTime _) = error "safeFromSql: cannot convert SqlDiffTime to Bool"
    safeFromSql (SqlPOSIXTime _) = error "safeFromSql: cannot convert SqlPOSIXTime to Bool"
    safeFromSql (SqlEpochTime x) = numToBool x
    safeFromSql (SqlTimeDiff x) = numToBool x
    safeFromSql (SqlNull) = error "safeFromSql: cannot convert SqlNull to Bool"

numToBool :: Num a => a -> Bool
numToBool x = x /= 0

instance SqlType Char where
    toSql = SqlChar
    safeFromSql (SqlString [x]) = x
    safeFromSql (SqlByteString x) = (head . byteString2String) x
    safeFromSql (SqlString _) = error "safeFromSql: cannot convert SqlString to Char"
    safeFromSql (SqlInt32 _) = error "safeFromSql: cannot convert SqlInt32 to Char"
    safeFromSql (SqlInt64 _) = error "safeFromSql: cannot convert SqlInt64 to Char"
    safeFromSql (SqlWord32 _) = error "safeFromSql: cannot convert SqlWord32 to Char"
    safeFromSql (SqlWord64 _) = error "safeFromSql: cannot convert SqlWord64 to Char"
    safeFromSql (SqlInteger _) = error "safeFromSql: cannot convert SqlInt to Char"
    safeFromSql (SqlChar x) = x
    safeFromSql (SqlBool x) = if x then '1' else '0'
    safeFromSql (SqlDouble _) = error "safeFromSql: cannot convert SqlDouble to Char"
    safeFromSql (SqlRational _) = error "safeFromSql: cannot convert SqlRational to Char"
    safeFromSql (SqlLocalDate _) = error "safeFromSql: cannot convert SqlLocalDate to Char"
    safeFromSql (SqlLocalTimeOfDay _) = error "safeFromSql: cannot convert SqlLocalTimeOfDay to Char"
    safeFromSql (SqlLocalTime _) = error "safeFromSql: cannot convert SqlLocalTime to Char"
    safeFromSql (SqlZonedTime _) = error "safeFromSql: cannot convert SqlZonedTime to Char"
    safeFromSql (SqlUTCTime _) = error "safeFromSql: cannot convert SqlUTCTime to Char"
    safeFromSql (SqlDiffTime _) = error "safeFromSql: cannot convert SqlDiffTime to Char"
    safeFromSql (SqlPOSIXTime _) = error "safeFromSql: cannot convert SqlPOSIXTime to Char"
    safeFromSql (SqlEpochTime _) = error "safeFromSql: cannot convert SqlEpochTime to Char"
    safeFromSql (SqlTimeDiff _) = error "safeFromSql: cannot convert SqlTimeDiff to Char"
    safeFromSql (SqlNull) = error "safeFromSql: cannot convert SqlNull to Char"

instance SqlType Double where
    toSql = SqlDouble
    safeFromSql (SqlString x) = read' x
    safeFromSql (SqlByteString x) = (read' . byteString2String) x
    safeFromSql (SqlInt32 x) = fromIntegral x
    safeFromSql (SqlInt64 x) = fromIntegral x
    safeFromSql (SqlWord32 x) = fromIntegral x
    safeFromSql (SqlWord64 x) = fromIntegral x
    safeFromSql (SqlInteger x) = fromIntegral x
    safeFromSql (SqlChar x) = fromIntegral . ord $ x
    safeFromSql (SqlBool x) = if x then 1.0 else 0.0
    safeFromSql (SqlDouble x) = x
    safeFromSql (SqlRational x) = fromRational x
    safeFromSql y@(SqlLocalDate _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql (SqlLocalTimeOfDay x) = fromRational . toRational . timeOfDayToTime $ x
    safeFromSql (SqlLocalTime _) = error "safeFromSql: Impossible to convert SqlLocalTime (LocalTime) to a numeric type."
    safeFromSql (SqlZonedTime x) = fromRational . toRational . utcTimeToPOSIXSeconds . 
                               zonedTimeToUTC $ x
    safeFromSql (SqlUTCTime x) = fromRational . toRational . utcTimeToPOSIXSeconds $ x
    safeFromSql (SqlDiffTime x) = fromRational . toRational $ x
    safeFromSql (SqlPOSIXTime x) = fromRational . toRational $ x
    safeFromSql (SqlEpochTime x) = fromIntegral x
    safeFromSql (SqlTimeDiff x) = fromIntegral x
    safeFromSql (SqlNull) = error "safeFromSql: cannot convert SqlNull to Double"

instance SqlType Rational where
    toSql = SqlRational
    safeFromSql (SqlString x) = read' x
    safeFromSql (SqlByteString x) = (read' . byteString2String) x
    safeFromSql (SqlInt32 x) = fromIntegral x
    safeFromSql (SqlInt64 x) = fromIntegral x
    safeFromSql (SqlWord32 x) = fromIntegral x
    safeFromSql (SqlWord64 x) = fromIntegral x
    safeFromSql (SqlInteger x) = fromIntegral x
    safeFromSql (SqlChar x) = fromIntegral . ord $ x
    safeFromSql (SqlBool x) = fromIntegral $ ((safeFromSql (SqlBool x))::Int)
    safeFromSql (SqlDouble x) = toRational x
    safeFromSql (SqlRational x) = x
    safeFromSql y@(SqlLocalDate _) = fromIntegral ((safeFromSql y)::Integer)
    safeFromSql (SqlLocalTimeOfDay x) = toRational . timeOfDayToTime $ x
    safeFromSql (SqlLocalTime _) = error "safeFromSql: Impossible to convert SqlLocalTime (LocalTime) to a numeric type."
    safeFromSql (SqlZonedTime x) = toRational . utcTimeToPOSIXSeconds . zonedTimeToUTC $ x
    safeFromSql (SqlUTCTime x) = toRational . utcTimeToPOSIXSeconds $ x
    safeFromSql (SqlDiffTime x) = toRational x
    safeFromSql (SqlPOSIXTime x) = toRational x
    safeFromSql (SqlEpochTime x) = fromIntegral x
    safeFromSql (SqlTimeDiff x) = fromIntegral x
    safeFromSql (SqlNull) = error "safeFromSql: cannot convert SqlNull to Double"

instance SqlType Day where
    toSql = SqlLocalDate
    safeFromSql (SqlString x) = parseTime' "Day" (iso8601DateFormat Nothing) x
    safeFromSql y@(SqlByteString _) = safeFromSql (SqlString (safeFromSql y))
    safeFromSql (SqlInt32 x) = ModifiedJulianDay {toModifiedJulianDay = fromIntegral x}
    safeFromSql (SqlInt64 x) = ModifiedJulianDay {toModifiedJulianDay = fromIntegral x}
    safeFromSql (SqlWord32 x) = ModifiedJulianDay {toModifiedJulianDay = fromIntegral x}
    safeFromSql (SqlWord64 x) = ModifiedJulianDay {toModifiedJulianDay = fromIntegral x}
    safeFromSql (SqlInteger x) = ModifiedJulianDay {toModifiedJulianDay = x}
    safeFromSql (SqlChar _) = error "safeFromSql: cannot convert SqlChar to Day"
    safeFromSql (SqlBool _) = error "safeFromSql: cannot convert SqlBool to Day"
    safeFromSql (SqlDouble x) = ModifiedJulianDay {toModifiedJulianDay = truncate x}
    safeFromSql (SqlRational x) = safeFromSql . SqlDouble . fromRational $ x
    safeFromSql (SqlLocalDate x) = x
    safeFromSql (SqlLocalTimeOfDay _) = error "x"
    safeFromSql (SqlLocalTime x) = localDay x
    safeFromSql (SqlZonedTime x) = localDay . zonedTimeToLocalTime $ x
    safeFromSql y@(SqlUTCTime _) = localDay . zonedTimeToLocalTime . safeFromSql $ y
    safeFromSql (SqlDiffTime _) = error "safeFromSql: cannot convert SqlDiffTime to Day"
    safeFromSql y@(SqlPOSIXTime _) = localDay . zonedTimeToLocalTime . safeFromSql $ y
    safeFromSql y@(SqlEpochTime _) = localDay . zonedTimeToLocalTime . safeFromSql $ y
    safeFromSql (SqlTimeDiff _) = error "safeFromSql: cannot convert SqlTimeDiff to Day"
    safeFromSql (SqlNull) = error "safeFromSql: cannot convert SqlNull to Day"

instance SqlType TimeOfDay where
    toSql = SqlLocalTimeOfDay
    safeFromSql (SqlString x) = parseTime' "TimeOfDay" "%T" x
    safeFromSql y@(SqlByteString _) = safeFromSql (SqlString (safeFromSql y))
    safeFromSql (SqlInt32 x) = timeToTimeOfDay . fromIntegral $ x
    safeFromSql (SqlInt64 x) = timeToTimeOfDay . fromIntegral $ x
    safeFromSql (SqlWord32 x) = timeToTimeOfDay . fromIntegral $ x
    safeFromSql (SqlWord64 x) = timeToTimeOfDay . fromIntegral $ x
    safeFromSql (SqlInteger x) = timeToTimeOfDay . fromInteger $ x
    safeFromSql (SqlChar _) = error "safeFromSql: cannot convert SqlChar to TimeOfDay"
    safeFromSql (SqlBool _) = error "safeFromSql: cannot convert SqlBool to TimeOfDay"
    safeFromSql (SqlDouble x) = timeToTimeOfDay . fromIntegral $ 
                            ((truncate x)::Integer)
    safeFromSql (SqlRational x) = safeFromSql . SqlDouble . fromRational $ x
    safeFromSql (SqlLocalDate _) = error "safeFromSql: cannot convert SqlLocalDate to TimeOfDay"
    safeFromSql (SqlLocalTimeOfDay x) = x
    safeFromSql (SqlLocalTime x) = localTimeOfDay x
    safeFromSql (SqlZonedTime x) = localTimeOfDay . zonedTimeToLocalTime $ x
    safeFromSql y@(SqlUTCTime _) = localTimeOfDay . zonedTimeToLocalTime . safeFromSql $ y
    safeFromSql (SqlDiffTime _) = error "safeFromSql: cannot convert SqlDiffTime to TimeOfDay"
    safeFromSql y@(SqlPOSIXTime _) = localTimeOfDay . zonedTimeToLocalTime . safeFromSql $ y
    safeFromSql y@(SqlEpochTime _) = localTimeOfDay . zonedTimeToLocalTime . safeFromSql $ y
    safeFromSql (SqlTimeDiff _) = error "safeFromSql: cannot convert SqlTimeDiff to TimeOfDay"
    safeFromSql SqlNull = error "safeFromSql: cannot convert SqlNull to Day"

instance SqlType LocalTime where
    toSql = SqlLocalTime
    safeFromSql (SqlString x) = parseTime' "LocalTime" (iso8601DateFormat (Just "%T")) x
    safeFromSql y@(SqlByteString _) = safeFromSql (SqlString (safeFromSql y))
    safeFromSql (SqlInt32 _) = error "foo"
    safeFromSql (SqlInt64 _) = error "foo"
    safeFromSql (SqlWord32 _) = error "f"
    safeFromSql (SqlWord64 _) = error "f"
    safeFromSql (SqlInteger _) = error "safeFromSql: Impossible to convert SqlInteger to LocalTime"
    safeFromSql (SqlChar _) = error "f"
    safeFromSql (SqlBool _) = error "f"
    safeFromSql (SqlDouble _) = error "f"
    safeFromSql (SqlRational _) = error "f"
    safeFromSql (SqlLocalDate _) = error "f"
    safeFromSql (SqlLocalTimeOfDay _) = error "f"
    safeFromSql (SqlLocalTime x) = x
    safeFromSql (SqlZonedTime x) = zonedTimeToLocalTime x
    safeFromSql y@(SqlUTCTime _) = zonedTimeToLocalTime . safeFromSql $ y
    safeFromSql (SqlDiffTime _) = error "f"
    safeFromSql y@(SqlPOSIXTime _) = zonedTimeToLocalTime . safeFromSql $ y
    safeFromSql y@(SqlEpochTime _) = zonedTimeToLocalTime . safeFromSql $ y
    safeFromSql (SqlTimeDiff _) = error "f"
    safeFromSql SqlNull = error "f"

instance SqlType ZonedTime where
    toSql x = SqlZonedTime x
    safeFromSql (SqlString x) = parseTime' "ZonedTime" (iso8601DateFormat (Just "%T %z")) x
    safeFromSql (SqlByteString x) = safeFromSql (SqlString (byteString2String x))
    safeFromSql (SqlInt32 x) = safeFromSql (SqlInteger (fromIntegral x))
    safeFromSql (SqlInt64 x) = safeFromSql (SqlInteger (fromIntegral x))
    safeFromSql (SqlWord32 x) = safeFromSql (SqlInteger (fromIntegral x))
    safeFromSql (SqlWord64 x) = safeFromSql (SqlInteger (fromIntegral x))
    safeFromSql y@(SqlInteger _) = utcToZonedTime utc (safeFromSql y)
    safeFromSql (SqlChar _) = error "safeFromSql: cannot convert SqlChar to ZonedTime"
    safeFromSql (SqlBool _) = error "safeFromSql: cannot convert SqlBool to ZonedTime"
    safeFromSql y@(SqlDouble _) = utcToZonedTime utc (safeFromSql y)
    safeFromSql y@(SqlRational _) = utcToZonedTime utc (safeFromSql y)
    safeFromSql (SqlLocalDate _) = error "safeFromSql: cannot convert SqlLocalDate to ZonedTime"
    safeFromSql (SqlLocalTime _) = error "safeFromSql: cannot convert SqlLocalTime to ZonedTime"
    safeFromSql (SqlLocalTimeOfDay _) = error "safeFromSql: cannot convert SqlLocalTimeOfDay to ZonedTime"
    safeFromSql (SqlZonedTime x) = x
    safeFromSql (SqlUTCTime x) = utcToZonedTime utc x
    safeFromSql (SqlDiffTime _) = error "safeFromSql: cannot convert SqlDiffTime to ZonedTime"
    safeFromSql y@(SqlPOSIXTime _) = utcToZonedTime utc (safeFromSql y)
    safeFromSql y@(SqlEpochTime _) = utcToZonedTime utc (safeFromSql y)
    safeFromSql (SqlTimeDiff _) = error "safeFromSql: cannot convert SqlTimeDiff to ZonedTime"
    safeFromSql SqlNull = error "safeFromSql: cannot convert SqlNull to ZonedTime"

instance SqlType UTCTime where
    toSql = SqlUTCTime
    safeFromSql (SqlString x) = parseTime' "UTCTime" (iso8601DateFormat (Just "%T")) x
    safeFromSql (SqlByteString x) = safeFromSql (SqlString (byteString2String x))
    safeFromSql y@(SqlInt32 _) = posixSecondsToUTCTime . safeFromSql $ y
    safeFromSql y@(SqlInt64 _) = posixSecondsToUTCTime . safeFromSql $ y
    safeFromSql y@(SqlWord32 _) = posixSecondsToUTCTime . safeFromSql $ y
    safeFromSql y@(SqlWord64 _) = posixSecondsToUTCTime . safeFromSql $ y
    safeFromSql y@(SqlInteger _) = posixSecondsToUTCTime . safeFromSql $ y
    safeFromSql (SqlChar _) = error "safeFromSql: cannot convert SqlChar to UTCTime"
    safeFromSql (SqlBool _) = error "safeFromSql: cannot convert SqlBool to UTCTime"
    safeFromSql y@(SqlDouble _) = posixSecondsToUTCTime . safeFromSql $ y
    safeFromSql y@(SqlRational _) = posixSecondsToUTCTime . safeFromSql $ y
    safeFromSql (SqlLocalDate _) = error "safeFromSql: cannot convert SqlLocalDate to UTCTime"
    safeFromSql (SqlLocalTimeOfDay _) = error "safeFromSql: cannot convert SqlLocalTimeOfDay to UTCTime"
    safeFromSql (SqlLocalTime _) = error "safeFromSql: cannot convert SqlLocalTime to UTCTime"
    safeFromSql (SqlZonedTime x) = zonedTimeToUTC x
    safeFromSql (SqlUTCTime x) = x
    safeFromSql (SqlDiffTime _) = error "safeFromSql: cannot convert SqlDiffTime to UTCTime; did you mean SqlPOSIXTime?"
    safeFromSql (SqlPOSIXTime x) = posixSecondsToUTCTime x
    safeFromSql y@(SqlEpochTime _) = posixSecondsToUTCTime . safeFromSql $ y
    safeFromSql (SqlTimeDiff _) = error "safeFromSql: cannot convert SqlTimeDiff to UTCTime; did you mean SqlPOSIXTime?"
    safeFromSql SqlNull = error "safeFromSql: cannot convert SqlNull to UTCTime"

instance SqlType NominalDiffTime where
    toSql = SqlDiffTime
    safeFromSql (SqlString x) = fromInteger (read' x)
    safeFromSql (SqlByteString x) = fromInteger ((read' . byteString2String) x)
    safeFromSql (SqlInt32 x) = fromIntegral x
    safeFromSql (SqlInt64 x) = fromIntegral x
    safeFromSql (SqlWord32 x) = fromIntegral x
    safeFromSql (SqlWord64 x) = fromIntegral x
    safeFromSql (SqlInteger x) = fromIntegral x
    safeFromSql (SqlChar _) = error "safeFromSql: cannot convert SqlChar to NominalDiffTime"
    safeFromSql (SqlBool _) = error "safeFromSql: cannot convert SqlBool to NominalDiffTime"
    safeFromSql (SqlDouble x) = fromRational . toRational $ x
    safeFromSql (SqlRational x) = fromRational x
    safeFromSql (SqlLocalDate x) = fromIntegral . (\y -> y * 60 * 60 * 24) . 
                               toModifiedJulianDay $ x
    safeFromSql (SqlLocalTimeOfDay x) = fromRational . toRational . timeOfDayToTime $ x
    safeFromSql (SqlLocalTime _) = error "safeFromSql: cannot convert SqlLocalTime to NominalDiffTime"
    safeFromSql (SqlZonedTime x) = utcTimeToPOSIXSeconds . zonedTimeToUTC $ x
    safeFromSql (SqlUTCTime x) = utcTimeToPOSIXSeconds x
    safeFromSql (SqlDiffTime x) = x
    safeFromSql (SqlPOSIXTime x) = x
    safeFromSql (SqlEpochTime x) = fromIntegral x
    safeFromSql (SqlTimeDiff x) = fromIntegral x
    safeFromSql SqlNull = error "safeFromSql: cannot convert SqlNull to NominalDiffTime"

instance SqlType ST.ClockTime where
    toSql (ST.TOD x y) = SqlPOSIXTime . fromRational $ 
                                        fromInteger x + fromRational (y % 1000000000000)
    safeFromSql (SqlString x) = ST.TOD (read' x) 0
    safeFromSql (SqlByteString x) = ST.TOD ((read' . byteString2String) x) 0
    safeFromSql (SqlInt32 x) = ST.TOD (fromIntegral x) 0
    safeFromSql (SqlInt64 x) = ST.TOD (fromIntegral x) 0
    safeFromSql (SqlWord32 x) = ST.TOD (fromIntegral x) 0
    safeFromSql (SqlWord64 x) = ST.TOD (fromIntegral x) 0
    safeFromSql (SqlInteger x) = ST.TOD x 0
    safeFromSql (SqlChar _) = error "safeFromSql: cannot convert SqlChar to ClockTime"
    safeFromSql (SqlBool _) = error "safeFromSql: cannot convert SqlBool to ClockTime"
    safeFromSql (SqlDouble x) = ST.TOD (truncate x) 0
    safeFromSql (SqlRational x) = ST.TOD (truncate x) 0
    safeFromSql (SqlLocalDate _) = error "safeFromSql: cannot convert SqlLocalDate to ClockTime"
    safeFromSql (SqlLocalTimeOfDay _) = error "safeFromSql: cannot convert SqlLocalTimeOfDay to ClockTime"
    safeFromSql (SqlLocalTime _) = error "safeFromSql: cannot convert SqlLocalTime to ClockTime"
    safeFromSql y@(SqlZonedTime _) = ST.TOD (safeFromSql y) 0
    safeFromSql y@(SqlUTCTime _) = ST.TOD (safeFromSql y) 0
    safeFromSql (SqlDiffTime _) = error "safeFromSql: cannot convert SqlDiffTime to ClockTime"
    safeFromSql y@(SqlPOSIXTime _) = ST.TOD (safeFromSql y) 0
    safeFromSql (SqlEpochTime x) = ST.TOD x 0
    safeFromSql (SqlTimeDiff _) = error "safeFromSql: cannot convert SqlTimeDiff to ClockTime"
    safeFromSql SqlNull = error "safeFromSql: cannot convert SqlNull to ClockTime"

instance SqlType ST.TimeDiff where
    toSql x = SqlDiffTime . fromIntegral . timeDiffToSecs $ x
    safeFromSql (SqlString x) = secs2td (read' x)
    safeFromSql (SqlByteString x) = secs2td ((read' . byteString2String) x)
    safeFromSql (SqlInt32 x) = secs2td (fromIntegral x)
    safeFromSql (SqlInt64 x) = secs2td (fromIntegral x)
    safeFromSql (SqlWord32 x) = secs2td (fromIntegral x)
    safeFromSql (SqlWord64 x) = secs2td (fromIntegral x)
    safeFromSql (SqlInteger x) = secs2td x
    safeFromSql (SqlChar _) = error "safeFromSql: cannot convert SqlChar to TimeDiff"
    safeFromSql (SqlBool _) = error "safeFromSql: cannot convert SqlBool to TimeDiff"
    safeFromSql (SqlDouble x) = secs2td (truncate x)
    safeFromSql (SqlRational x) = secs2td (truncate x)
    safeFromSql (SqlLocalDate _) = error "safeFromSql: cannot convert SqlLocalDate to TimeDiff"
    safeFromSql (SqlLocalTimeOfDay _) = error "safeFromSql: cannot convert SqlLocalTimeOfDay to TimeDiff"
    safeFromSql (SqlLocalTime _) = error "safeFromSql: cannot convert SqlLocalTime to TimeDiff"
    safeFromSql (SqlZonedTime _) = error "safeFromSql: cannot convert SqlZonedTime to TimeDiff"
    safeFromSql (SqlUTCTime _) = error "safeFromSql: cannot convert SqlUTCTime to TimeDiff"
    safeFromSql (SqlPOSIXTime _) = error "safeFromSql: cannot convert SqlPOSIXTime to TimeDiff"
    safeFromSql (SqlDiffTime x) = secs2td (truncate x)
    safeFromSql (SqlEpochTime _) = error "safeFromSql: cannot convert SqlEpochTime to TimeDiff"
    safeFromSql (SqlTimeDiff x) = secs2td x
    safeFromSql SqlNull = error "safeFromSql: cannot convert SqlNull to TimeDiff"

instance SqlType DiffTime where
    toSql x = SqlDiffTime . fromRational . toRational $ x
    safeFromSql (SqlString x) = fromInteger (read' x)
    safeFromSql (SqlByteString x) = fromInteger ((read' . byteString2String) x)
    safeFromSql (SqlInt32 x) = fromIntegral x
    safeFromSql (SqlInt64 x) = fromIntegral x
    safeFromSql (SqlWord32 x) = fromIntegral x
    safeFromSql (SqlWord64 x) = fromIntegral x
    safeFromSql (SqlInteger x) = fromIntegral x
    safeFromSql (SqlChar _) = error "safeFromSql: cannot convert SqlChar to DiffTime"
    safeFromSql (SqlBool _) = error "safeFromSql: cannot convert SqlBool to DiffTime"
    safeFromSql (SqlDouble x) = fromIntegral ((truncate x)::Integer)
    safeFromSql (SqlRational x) = fromIntegral ((truncate x)::Integer)
    safeFromSql (SqlLocalDate _) = error "safeFromSql: cannot convert SqlLocalDate to DiffTime"
    safeFromSql (SqlLocalTimeOfDay _) = error "safeFromSql: cannot convert SqlLocalTimeOfDay to DiffTime"
    safeFromSql (SqlLocalTime _) = error "safeFromSql: cannot convert SqlLocalTime to DiffTime"
    safeFromSql (SqlZonedTime _) = error "safeFromSql: cannot convert SqlZonedTime to DiffTime"
    safeFromSql (SqlUTCTime _) = error "safeFromSql: cannot convert SqlUTCTime to DiffTime"
    safeFromSql (SqlDiffTime x) = fromRational . toRational $ x
    safeFromSql (SqlPOSIXTime _) = error "safeFromSql: cannot convert SqlPOSIXTime to DiffTime"
    safeFromSql (SqlEpochTime _) = error "safeFromSql: cannot convert SqlEpochTime to DiffTime"
    safeFromSql (SqlTimeDiff x) = fromIntegral x
    safeFromSql SqlNull = error "safeFromSql: cannot convert SqlNull to DiffTime"

instance SqlType ST.CalendarTime where
    toSql x = toSql (ST.toClockTime x)
    safeFromSql = ST.toUTCTime . safeFromSql
-}
instance (SqlType a) => SqlType (Maybe a) where
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

secs2td :: Integer -> ST.TimeDiff
secs2td x = ST.diffClockTimes (ST.TOD x 0) (ST.TOD 0 0)


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

parseTime' :: ParseTime t => String -> String -> String -> t
parseTime' t fmtstr inpstr = ret
    where ret = case parseTime defaultTimeLocale fmtstr inpstr of
                  Nothing -> error $ "fromSql: Cannot read " ++ show inpstr ++ " as " ++ 
                             t ++ " using default format string " ++ show fmtstr ++ "."
                  Just x -> x

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

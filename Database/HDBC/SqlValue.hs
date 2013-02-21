module Database.HDBC.SqlValue
    (
     -- * SQL value marshalling
     SqlValue(..),
     safeFromSql, toSql, fromSql,
     nToSql, iToSql
    )

where
import Data.Dynamic
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Char(ord,toUpper)
import Data.Word
import Data.Int
import Data.Decimal
import Data.UUID
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
data SqlValue =
  {- | Arbitrary precision value -}
  SqlDecimal Decimal
  | SqlWord32 Word32
  | SqlWord64 Word64
  | SqlInt32 Int32
  | SqlInt64 Int64
  | SqlInteger Integer
  | SqlDouble Double
  | SqlString String
  | SqlByteString B.ByteString
  | SqlBool Bool
    {- | Represent bit field with 64 bits -}
  | SqlBitField Word64
    {- | UUID value http://en.wikipedia.org/wiki/UUID -}
  | SqlUUID UUID

  | SqlUTCTime UTCTime          -- ^ UTC YYYY-MM-DD HH:MM:SS
  | SqlLocalDate Day            -- ^ Local YYYY-MM-DD (no timezone)
  | SqlLocalTimeOfDay TimeOfDay -- ^ Local HH:MM:SS (no timezone)
  | SqlLocalTime LocalTime      -- ^ Local YYYY-MM-DD HH:MM:SS (no timezone)

    {- | The value of current datetime on the server.
      Different database drivers will convert it to the appropriate literal/function.
      Not used for retriving data from the database, just for writing to.
    -}
  | SqlNow
  | SqlNull         -- ^ NULL in SQL or Nothing in Haskell
  deriving (Show)

instance Typeable SqlValue where
    typeOf _ = mkTypeName "SqlValue"

instance Eq SqlValue where

    (SqlDecimal a)        == (SqlDecimal b)         = a == b
    (SqlWord32 a)         == (SqlWord32 b)          = a == b
    (SqlWord64 a)         == (SqlWord64 b)          = a == b
    (SqlInt32 a)          == (SqlInt32 b)           = a == b
    (SqlInt64 a)          == (SqlInt64 b)           = a == b
    (SqlInteger a)        == (SqlInteger b)         = a == b
    (SqlDouble a)         == (SqlDouble b)          = a == b
    (SqlString a)         == (SqlString b)          = a == b
    (SqlByteString a)     == (SqlByteString b)      = a == b
    (SqlBool a)           == (SqlBool b)            = a == b
    (SqlBitField a)       == (SqlBitField b)        = a == b
    (SqlUUID a)           == (SqlUUID b)            = a == b
    (SqlUTCTime a)        == (SqlUTCTime b)         = a == b
    (SqlLocalDate a)      == (SqlLocalDate b)       = a == b
    (SqlLocalTimeOfDay a) == (SqlLocalTimeOfDay b)  = a == b
    (SqlLocalTime a)      == (SqlLocalTime b)       = a == b
    SqlNow == SqlNow = False     -- Concrete value will be determined on the database
    _ == SqlNow = False
    SqlNow == _ = False
    SqlNull == SqlNull = True
    SqlNull == _ = False
    _ == SqlNull = False
    a == b = case convres of
      Left _ -> False
      Right r -> r
      where
        convres = do
          x <- ((safeFromSql a)::ConvertResult String)
          y <- ((safeFromSql b)::ConvertResult String)
          return $ x == y

instance Convertible SqlValue SqlValue where
    safeConvert = return

instance Convertible String SqlValue where
    safeConvert = return . SqlString
instance Convertible SqlValue String where

  safeConvert (SqlDecimal a)        = return $ show a
  safeConvert (SqlWord32 a)         = return $ show a
  safeConvert (SqlWord64 a)         = return $ show a
  safeConvert (SqlInt32 a)          = return $ show a
  safeConvert (SqlInt64 a)          = return $ show a
  safeConvert (SqlInteger a)        = return $ show a
  safeConvert (SqlDouble a)         = return $ show a
  safeConvert (SqlString a)         = return a
  safeConvert (SqlByteString x)     = return . BUTF8.toString $ x
  safeConvert (SqlBool a)           = return $ show a
  safeConvert (SqlBitField a)       = return $ show a
  safeConvert (SqlUUID a)           = return $ show a
  safeConvert (SqlUTCTime a)        = return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q")) $ a
  safeConvert (SqlLocalDate a)      = return . formatTime defaultTimeLocale (iso8601DateFormat Nothing) $ a
  safeConvert (SqlLocalTimeOfDay a) = return . formatTime defaultTimeLocale "%T%Q" $ a
  safeConvert (SqlLocalTime a)      = return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q")) $ a
  safeConvert x@SqlNow  = quickError x
  safeConvert x@SqlNull = quickError x

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
    safeConvert x = fmap SqlInt64 $ safeConvert x
instance Convertible SqlValue Int where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlWord32 a)           = safeConvert a
  safeConvert (SqlWord64 a)           = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Int32 SqlValue where
    safeConvert = return . SqlInt32
instance Convertible SqlValue Int32 where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlWord32 a)           = safeConvert a
  safeConvert (SqlWord64 a)           = safeConvert a
  safeConvert (SqlInt32 a)            = return a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Int64 SqlValue where
    safeConvert = return . SqlInt64
instance Convertible SqlValue Int64 where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlWord32 a)           = safeConvert a
  safeConvert (SqlWord64 a)           = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = return a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Word32 SqlValue where
    safeConvert = return . SqlWord32
instance Convertible SqlValue Word32 where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlWord32 a)           = return a
  safeConvert (SqlWord64 a)           = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Word64 SqlValue where
    safeConvert = return . SqlWord64
instance Convertible SqlValue Word64 where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlWord32 a)           = safeConvert a
  safeConvert (SqlWord64 a)           = return a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = return a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Integer SqlValue where
    safeConvert = return . SqlInteger
instance Convertible SqlValue Integer where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlWord32 a)           = safeConvert a
  safeConvert (SqlWord64 a)           = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = return a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Bool SqlValue where
    safeConvert = return . SqlBool
instance Convertible SqlValue Bool where
  safeConvert (SqlDecimal a)          = numToBool a
  safeConvert (SqlWord32 a)           = numToBool a
  safeConvert (SqlWord64 a)           = numToBool a
  safeConvert (SqlInt32 a)            = numToBool a
  safeConvert (SqlInt64 a)            = numToBool a
  safeConvert (SqlInteger a)          = numToBool a
  safeConvert (SqlDouble a)           = numToBool a
  safeConvert y@(SqlString x) =
    case map toUpper x of
      "TRUE" -> Right True
      "T" -> Right True
      "FALSE" -> Right False
      "F" -> Right False
      "0" -> Right False
      "1" -> Right True
      _ -> convError "Cannot parse given String as Bool" y
  safeConvert (SqlByteString x)       = (safeConvert . SqlString . BUTF8.toString) x
  safeConvert (SqlBool a)             = return a
  safeConvert (SqlBitField a)         = numToBool a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to Bool has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

numToBool :: (Eq a, Num a) => a -> ConvertResult Bool
numToBool x = Right (x /= 0)

instance Convertible Double SqlValue where
    safeConvert = return . SqlDouble
instance Convertible SqlValue Double where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlWord32 a)           = safeConvert a
  safeConvert (SqlWord64 a)           = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = return a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to Double has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Decimal SqlValue where
  safeConvert = return . SqlDecimal
instance Convertible SqlValue Decimal where
  safeConvert (SqlDecimal a)          = return a
  safeConvert (SqlWord32 a)           = safeConvert a
  safeConvert (SqlWord64 a)           = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlString a)           = read' a
  safeConvert (SqlByteString x)       = (read' . BUTF8.toString) x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to Double has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@(SqlNull)             = quickError y
  

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
  [(a, "")] -> Just a
  _         -> Nothing

readRational :: String -> ConvertResult Rational -- more smart reader for Rationals
readRational s = case reads s of
  [(a, "")] -> return a
  _ -> case decread of
    Just a -> return a
    Nothing -> convError "Could not read as Rational: " s
  where
    decread = do
      h <- readMay $ high ++ low
      return $ h % (10^lowdecs)
    (high, loW) = span (/= '.') s
    low = case drop 1 loW of
      "" -> "0"
      x -> x
    lowdecs = length $ dropWhile (== '0') $ reverse low -- drop tail zeros

#if ! (MIN_VERSION_time(1,1,3))
instance Typeable Day where
    typeOf _ = mkTypeName "Day"
instance Typeable TimeOfDay where
    typeOf _ = mkTypeName "TimeOfDay"
instance Typeable LocalTime where
    typeOf _ = mkTypeName "LocalTime"
#endif

instance Convertible Day SqlValue where
    safeConvert = return . SqlLocalDate
instance Convertible SqlValue Day where
  safeConvert x@(SqlDecimal _)                   = quickError x -- converting number to date has no sense
  safeConvert x@(SqlWord32 _)                    = quickError x
  safeConvert x@(SqlWord64 _)                    = quickError x
  safeConvert x@(SqlInt32 _)                     = quickError x
  safeConvert x@(SqlInt64 _)                     = quickError x
  safeConvert x@(SqlInteger _)                   = quickError x
  safeConvert x@(SqlDouble _)                    = quickError x
  safeConvert (SqlString x)                      = parseTime' (iso8601DateFormat Nothing) x
  safeConvert (SqlByteString x)                  = safeConvert (SqlString (BUTF8.toString x))
  safeConvert x@(SqlBool _)                      = quickError x
  safeConvert x@(SqlBitField _)                  = quickError x
  safeConvert x@(SqlUUID _)                      = quickError x
  safeConvert x@(SqlUTCTime _)                   = quickError x -- converting UTC time to local day has no sense, you must convert it to LocalTime explicitly giving TimeZone
  safeConvert (SqlLocalDate a)                   = return a
  safeConvert x@(SqlLocalTimeOfDay _)            = quickError x
  safeConvert (SqlLocalTime (LocalTime {localDay = a})) = return a
  safeConvert x@SqlNow                           = quickError x
  safeConvert y@(SqlNull)                        = quickError y

instance Convertible TimeOfDay SqlValue where
    safeConvert = return . SqlLocalTimeOfDay
instance Convertible SqlValue TimeOfDay where
  safeConvert x@(SqlDecimal _)                         = quickError x -- converting number to time has no sense
  safeConvert x@(SqlWord32 _)                          = quickError x
  safeConvert x@(SqlWord64 _)                          = quickError x
  safeConvert x@(SqlInt32 _)                           = quickError x
  safeConvert x@(SqlInt64 _)                           = quickError x
  safeConvert x@(SqlInteger _)                         = quickError x
  safeConvert x@(SqlDouble _)                          = quickError x
  safeConvert (SqlString x)                            = parseTime' "%T%Q" x
  safeConvert (SqlByteString x)                        = safeConvert (SqlString (BUTF8.toString x))
  safeConvert x@(SqlBool _)                            = quickError x
  safeConvert x@(SqlBitField _)                        = quickError x
  safeConvert x@(SqlUUID _)                            = quickError x
  safeConvert x@(SqlUTCTime _)                         = quickError x -- converting UTC time to TimeOfDay has no sense, you must convert it to LocalTime explicitly giving TimeZone
  safeConvert x@(SqlLocalDate _)                       = quickError x
  safeConvert (SqlLocalTimeOfDay a)                    = return a
  safeConvert (SqlLocalTime (LocalTime {localTimeOfDay = a})) = return a
  safeConvert x@SqlNow                                 = quickError x
  safeConvert y@(SqlNull)                              = quickError y

instance Convertible LocalTime SqlValue where
    safeConvert = return . SqlLocalTime
instance Convertible SqlValue LocalTime where
  safeConvert x@(SqlDecimal _)        = quickError x -- converting number to time of day has no sense
  safeConvert x@(SqlWord32 _)         = quickError x
  safeConvert x@(SqlWord64 _)         = quickError x
  safeConvert x@(SqlInt32 _)          = quickError x
  safeConvert x@(SqlInt64 _)          = quickError x
  safeConvert x@(SqlInteger _)        = quickError x
  safeConvert x@(SqlDouble _)         = quickError x
  safeConvert (SqlString x)           = parseTime' (iso8601DateFormat (Just "%T%Q")) x
  safeConvert (SqlByteString x)       = safeConvert (SqlString (BUTF8.toString x))
  safeConvert x@(SqlBool _)           = quickError x
  safeConvert x@(SqlBitField _)       = quickError x
  safeConvert x@(SqlUUID _)           = quickError x
  safeConvert x@(SqlUTCTime _)        = quickError x -- converting UTC time to TimeOfDay has no sense, you must convert it to LocalTime explicitly giving TimeZone
  safeConvert (SqlLocalDate d)        = return $ LocalTime d midnight
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert (SqlLocalTime a)        = return a
  safeConvert x@SqlNow                = quickError x
  safeConvert y@SqlNull               = quickError y

instance Convertible UTCTime SqlValue where
    safeConvert = return . SqlUTCTime
instance Convertible SqlValue UTCTime where
  safeConvert x@(SqlDecimal _)        = quickError x -- converting number to UTC has no sense
  safeConvert x@(SqlWord32 _)         = quickError x
  safeConvert x@(SqlWord64 _)         = quickError x
  safeConvert x@(SqlInt32 _)          = quickError x
  safeConvert x@(SqlInt64 _)          = quickError x
  safeConvert x@(SqlInteger _)        = quickError x
  safeConvert x@(SqlDouble _)         = quickError x
  safeConvert (SqlString x)           = parseTime' (iso8601DateFormat (Just "%T%Q")) x
  safeConvert (SqlByteString x)       = safeConvert (SqlString (BUTF8.toString x))
  safeConvert x@(SqlBool _)           = quickError x
  safeConvert x@(SqlBitField _)       = quickError x
  safeConvert x@(SqlUUID _)           = quickError x
  safeConvert (SqlUTCTime a)          = return a
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert x@SqlNow                = quickError x
  safeConvert y@SqlNull               = quickError y

stringToFixed :: (HasResolution r) => String -> ConvertResult (Fixed r)
stringToFixed s = fmap fromRational $ readRational s

stringToPico :: String -> ConvertResult Pico
stringToPico = stringToFixed

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
    case [x | (x, "") <- reads s] of
      [x] -> Right x
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

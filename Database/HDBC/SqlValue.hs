{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , OverloadedStrings
  #-}

#if ! (MIN_VERSION_time(1,1,3))
{-# LANGUAGE
    StandaloneDeriving #-} 
#endif

module Database.HDBC.SqlValue
    (
      -- * Convertion functions
      toSql
    , safeFromSql
    , fromSql
      -- * SQL value marshalling
    , SqlValue(..)
    )

where
import Data.Dynamic
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Data.Int
import Data.Decimal
import Data.UUID (UUID, fromString, toString)
import Data.Time
import System.Locale (defaultTimeLocale)
import Data.Convertible
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
                

{- | 'SqlValue' is the main type for expressing Haskell values to SQL databases.

/WHAT IS SQLVALUE/

SqlValue is an intermediate type to store/recevie data to/from the
database. Every database driver will do it's best to properly convert any
SqlValue to the database record's field, and properly convert the record's field
to SqlValue back.

The 'SqlValue' has predefined 'Convertible' instances for many Haskell's
types. Any Haskell's type can be converted to the 'SqlValue' with
'Database.HDBC.Utils.toSql' function. There is no safeToSql function because
toSql never fails. Also, any 'SqlValue' type can be converted to almost any
Haskell's type as well. Not any 'SqlValue' can be converted back to Haskell's
type, so there is 'Database.HDBC.Utils.safeFromSql' function to do that
safely. There is unsafe 'Database.HDBC.Utils.toSql' function of caurse.

You can sure, that @fromSql . toSql == id@

/SQLVALUE CONSTRUCTORS/

SqlValue constructors is the MINIMAL set of constructors, required to represent
the most wide range of native database types.

For example, there is FLOAT native database type and DOUBLE, but any DOUBLE can
carry any FLOAT value, so there is no need to create 'SqlValue' constructor to
represent FLOAT type, we can do it with Double. But there is DECIMAL database
type, representing arbitrary precision value which can be carried just by
'Decimal' Haskell's type, so we need a constructor for it.

There is no SqlRational any more, because there is no one database which have
native Rational type. This is the key idea: if database can not store this type
natively we will not create 'SqlValue' clause for it.

Each SqlValue constructor is documented or self-explaining to understand what it
is needed for.

/CONVERTIBLE INSTANCES/

The key idea is to do the most obvious conversion between types only if it is
not ambiguous. For example, the most obvious conversion of Double to Int32 is
just truncate the Double, the most obvious conversion of String to UTCTime is to
try read the String as date and time. But there is no obvious way to convert
Int32 to UTCTime, so if you will try to convert (SqlInt32 44) to date you will
fail. User must handle this cases properly converting values with right way, it
is not very good idea to silently perform strange and ambiguous convertions
between absolutely different data types.

/ERROR CONDITIONS/

There may sometimes be an error during conversion.  For instance, if you have a
'SqlString' and are attempting to convert it to an Integer, but it doesn't parse
as an Integer, you will get an error.  This will be indicated as an exception if
using 'Database.HDBC.Utils.fromSql', or a Left result if using
'Database.HDBC.Utils.safeFromSql'.


/STORING SQLVALUE TO DATABASE/

Any SqlValue can be converted to Text and then readed from Text back. This is
guaranteed by tests, so the database driver's author can use it to store and
read data through Text for types which is not supported by the database
natively.

/TEXT AND BYTESTRINGS/

We are using lazy Text everywhere because it is faster than String and has
builders. Strict text can be converted to one-chanked lazy text with O(1)
complexity, but lazy to strict converts with O(n) complexity, so it is logical
to use lazy Text.

We are not using ByteString as text encoded in UTF-8, ByteStrings are just
sequences of bytes. We are using strict ByteStrings because HDBC drivers uses
them to pass the ByteString to the C library as CString, so it must be strict.

We are not using String as data of query or as query itself because it is not
effective by memory and cpu.

/DATE AND TIME/

We are not using time with timezone, because there is no one database working
with it natively except PostgreSQL, but the documentations of PostgreSQL says

/To address these difficulties, we recommend using date/time types that contain
both date and time when using time zones. We do not recommend using the type
time with time zone (though it is supported by PostgreSQL for legacy
applications and for compliance with the SQL standard). PostgreSQL assumes your
local time zone for any type containing only date or time./

This is not recomended to use time with timezone.

We are using UTCTime instead of TimeWithTimezone because no one database
actually save timezone information. All databases just convert datetime to
UTCTime when save data and convert UTCTime back to LOCAL SERVER TIMEZONE when
returning the data. So it is logical to work with timezones on the haskell side.

Time intervals are not widely supported, actually just PostgreSQL and
Oracle. So, if you need them you can serialize throgh SqlText by hands, or write
your own Convertible instances to do that more convenient.

/EQUALITY OF SQLVALUE/

Two SqlValues are considered to be equal if one of these hold.  The
first comparison that can be made is controlling; if none of these
comparisons can be made, then they are not equal:

 * Both are NULL

 * Both represent the same type and the encapsulated values are considered equal
   by applying (==) to them

 * The values of each, when converted to a string, are equal.

-}
data SqlValue =
  {- | Arbitrary precision DECIMAL value -}
  SqlDecimal Decimal
  | SqlInt32 Int32
  | SqlInt64 Int64
  | SqlInteger Integer
  | SqlDouble Double
  | SqlText TL.Text
    -- | Blob field in the database. This field can not be implicitly converted
    -- to any other type because it is just an array of bytes, not an UTF-8
    -- encoded string.
  | SqlBlob B.ByteString
  | SqlBool Bool
    {- | Represent bit field with 64 bits -}
  | SqlBitField Word64
    {- | UUID value http://en.wikipedia.org/wiki/UUID -}
  | SqlUUID UUID

  | SqlUTCTime UTCTime          -- ^ UTC YYYY-MM-DD HH:MM:SS
  | SqlLocalDate Day            -- ^ Local YYYY-MM-DD (no timezone)
  | SqlLocalTimeOfDay TimeOfDay -- ^ Local HH:MM:SS (no timezone)
  | SqlLocalTime LocalTime      -- ^ Local YYYY-MM-DD HH:MM:SS (no timezone)
  | SqlNull         -- ^ NULL in SQL or Nothing in Haskell
  deriving (Show, Typeable, Ord)

instance Eq SqlValue where

    (SqlDecimal a)        == (SqlDecimal b)         = a == b
    (SqlInt32 a)          == (SqlInt32 b)           = a == b
    (SqlInt64 a)          == (SqlInt64 b)           = a == b
    (SqlInteger a)        == (SqlInteger b)         = a == b
    (SqlDouble a)         == (SqlDouble b)          = a == b
    (SqlText a)           == (SqlText b)            = a == b
    (SqlBlob a)           == (SqlBlob b)            = a == b
    (SqlBool a)           == (SqlBool b)            = a == b
    (SqlBitField a)       == (SqlBitField b)        = a == b
    (SqlUUID a)           == (SqlUUID b)            = a == b
    (SqlUTCTime a)        == (SqlUTCTime b)         = a == b
    (SqlLocalDate a)      == (SqlLocalDate b)       = a == b
    (SqlLocalTimeOfDay a) == (SqlLocalTimeOfDay b)  = a == b
    (SqlLocalTime a)      == (SqlLocalTime b)       = a == b
    SqlNull == SqlNull = True
    SqlNull == _ = False
    _ == SqlNull = False
    a == b = case convres of
      Left _ -> False
      Right r -> r
      where
        convres = do
          x <- (safeConvert a) :: ConvertResult String
          y <- (safeConvert b) :: ConvertResult String
          return $ x == y

instance Convertible SqlValue SqlValue where
    safeConvert = return

instance Convertible [Char] SqlValue where
    safeConvert = return . SqlText . TL.pack
instance Convertible SqlValue [Char] where

  safeConvert (SqlDecimal a)        = return $ show a
  safeConvert (SqlInt32 a)          = return $ show a
  safeConvert (SqlInt64 a)          = return $ show a
  safeConvert (SqlInteger a)        = return $ show a
  safeConvert (SqlDouble a)         = return $ show a
  safeConvert (SqlText a)           = return $ TL.unpack a
  safeConvert x@(SqlBlob _)         = quickError x -- bytes is not a text
  safeConvert (SqlBool a)           = return $ show a
  safeConvert (SqlBitField a)       = return $ show a
  safeConvert (SqlUUID a)           = return $ toString a
  safeConvert (SqlUTCTime a)        = return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q")) $ a
  safeConvert (SqlLocalDate a)      = return . formatTime defaultTimeLocale (iso8601DateFormat Nothing) $ a
  safeConvert (SqlLocalTimeOfDay a) = return . formatTime defaultTimeLocale "%T%Q" $ a
  safeConvert (SqlLocalTime a)      = return . formatTime defaultTimeLocale (iso8601DateFormat (Just "%T%Q")) $ a
  safeConvert x@SqlNull = quickError x

instance Convertible TS.Text SqlValue where
    safeConvert = return . SqlText . TL.fromChunks . (:[])

instance Convertible SqlValue TS.Text where
  safeConvert (SqlText t) = return $ TL.toStrict t
  safeConvert x = fmap TS.pack $ safeConvert x

instance Convertible TL.Text SqlValue where
    safeConvert = return . SqlText 

instance Convertible SqlValue TL.Text where
  safeConvert (SqlText t) = return t
  safeConvert x = fmap (TL.fromChunks . (:[]) . TS.pack) $ safeConvert x

instance Convertible B.ByteString SqlValue where
    safeConvert = return . SqlBlob
instance Convertible SqlValue B.ByteString where
    safeConvert (SqlBlob x) = return x
    safeConvert x = quickError x -- there is no sense to convert something to bytes except bytes

instance Convertible BSL.ByteString SqlValue where
    safeConvert = fmap SqlBlob . safeConvert
instance Convertible SqlValue BSL.ByteString where
    safeConvert (SqlBlob x) = safeConvert x
    safeConvert x = quickError x

instance Convertible UUID SqlValue where
  safeConvert x = return $ SqlUUID x

instance Convertible SqlValue UUID where
  safeConvert (SqlUUID u) = return u
  safeConvert (SqlText t) = case fromString $ TL.unpack t of
    Nothing  -> convError ("Could not convert " ++ (TL.unpack t) ++ " to UUID") t
    Just r -> return r
  safeConvert x = quickError x

instance Convertible Int SqlValue where
    safeConvert x = fmap SqlInt64 $ safeConvert x
instance Convertible SqlValue Int where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlText a)             = read' a
  safeConvert x@(SqlBlob _)           = quickError x -- Why should we read this bytes as UTF-8 ?
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Int32 SqlValue where
    safeConvert = return . SqlInt32
instance Convertible SqlValue Int32 where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlInt32 a)            = return a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlText a)             = read' a
  safeConvert x@(SqlBlob _)           = quickError x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Int64 SqlValue where
    safeConvert = return . SqlInt64
instance Convertible SqlValue Int64 where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = return a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlText a)             = read' a
  safeConvert x@(SqlBlob _)           = quickError x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Integer SqlValue where
    safeConvert = return . SqlInteger
instance Convertible SqlValue Integer where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = return a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlText a)             = read' a
  safeConvert x@(SqlBlob _)           = quickError x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to int has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Bool SqlValue where
    safeConvert = return . SqlBool
instance Convertible SqlValue Bool where
  safeConvert (SqlDecimal a)          = numToBool a
  safeConvert (SqlInt32 a)            = numToBool a
  safeConvert (SqlInt64 a)            = numToBool a
  safeConvert (SqlInteger a)          = numToBool a
  safeConvert (SqlDouble a)           = numToBool a
  safeConvert y@(SqlText x) =
    case TL.toUpper x of
      "TRUE"  -> Right True
      "T"     -> Right True
      "FALSE" -> Right False
      "F"     -> Right False
      "0"     -> Right False
      "1"     -> Right True
      _       -> convError "Cannot parse given String as Bool" y
  safeConvert x@(SqlBlob _)           = quickError x
  safeConvert (SqlBool a)             = return a
  safeConvert (SqlBitField a)         = numToBool a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to Bool has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert y@(SqlNull)             = quickError y

numToBool :: (Eq a, Num a) => a -> ConvertResult Bool
numToBool x = Right (x /= 0)

instance Convertible Double SqlValue where
    safeConvert = return . SqlDouble
instance Convertible SqlValue Double where
  safeConvert (SqlDecimal a)          = safeConvert a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = return a
  safeConvert (SqlText a)             = read' a
  safeConvert x@(SqlBlob _)           = quickError x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to Double has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert y@(SqlNull)             = quickError y

instance Convertible Decimal SqlValue where
  safeConvert = return . SqlDecimal
instance Convertible SqlValue Decimal where
  safeConvert (SqlDecimal a)          = return a
  safeConvert (SqlInt32 a)            = safeConvert a
  safeConvert (SqlInt64 a)            = safeConvert a
  safeConvert (SqlInteger a)          = safeConvert a
  safeConvert (SqlDouble a)           = safeConvert a
  safeConvert (SqlText a)             = read' a
  safeConvert x@(SqlBlob _)           = quickError x
  safeConvert (SqlBool a)             = return $ if a then 1 else 0
  safeConvert (SqlBitField a)         = safeConvert a
  safeConvert x@(SqlUUID _)           = quickError x -- converting time or date to Double has no sense
  safeConvert x@(SqlUTCTime _)        = quickError x
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert y@(SqlNull)             = quickError y



#if ! (MIN_VERSION_time(1,1,3))
    -- older versions if time had no Typeable instances
deriving instance Typeable Day
deriving instance Typeable TimeOfDay
deriving instance Typeable LocalTime
#endif

instance Convertible Day SqlValue where
    safeConvert = return . SqlLocalDate
instance Convertible SqlValue Day where
  safeConvert x@(SqlDecimal _)                   = quickError x -- converting number to date has no sense
  safeConvert x@(SqlInt32 _)                     = quickError x
  safeConvert x@(SqlInt64 _)                     = quickError x
  safeConvert x@(SqlInteger _)                   = quickError x
  safeConvert x@(SqlDouble _)                    = quickError x
  safeConvert (SqlText x)                        = parseTime' (iso8601DateFormat Nothing) $ TL.unpack x
  safeConvert x@(SqlBlob _)                      = quickError x
  safeConvert x@(SqlBool _)                      = quickError x
  safeConvert x@(SqlBitField _)                  = quickError x
  safeConvert x@(SqlUUID _)                      = quickError x
  safeConvert x@(SqlUTCTime _)                   = quickError x -- converting UTC time to local day has no sense, you must convert it to LocalTime explicitly giving TimeZone
  safeConvert (SqlLocalDate a)                   = return a
  safeConvert x@(SqlLocalTimeOfDay _)            = quickError x
  safeConvert (SqlLocalTime (LocalTime {localDay = a})) = return a
  safeConvert y@(SqlNull)                        = quickError y

instance Convertible TimeOfDay SqlValue where
    safeConvert = return . SqlLocalTimeOfDay
instance Convertible SqlValue TimeOfDay where
  safeConvert x@(SqlDecimal _)                         = quickError x -- converting number to time has no sense
  safeConvert x@(SqlInt32 _)                           = quickError x
  safeConvert x@(SqlInt64 _)                           = quickError x
  safeConvert x@(SqlInteger _)                         = quickError x
  safeConvert x@(SqlDouble _)                          = quickError x
  safeConvert (SqlText x)                              = parseTime' "%T%Q" $ TL.unpack x
  safeConvert x@(SqlBlob _)                            = quickError x
  safeConvert x@(SqlBool _)                            = quickError x
  safeConvert x@(SqlBitField _)                        = quickError x
  safeConvert x@(SqlUUID _)                            = quickError x
  safeConvert x@(SqlUTCTime _)                         = quickError x -- converting UTC time to TimeOfDay has no sense, you must convert it to LocalTime explicitly giving TimeZone
  safeConvert x@(SqlLocalDate _)                       = quickError x
  safeConvert (SqlLocalTimeOfDay a)                    = return a
  safeConvert (SqlLocalTime (LocalTime {localTimeOfDay = a})) = return a
  safeConvert y@(SqlNull)                              = quickError y

instance Convertible LocalTime SqlValue where
    safeConvert = return . SqlLocalTime
instance Convertible SqlValue LocalTime where
  safeConvert x@(SqlDecimal _)        = quickError x -- converting number to time of day has no sense
  safeConvert x@(SqlInt32 _)          = quickError x
  safeConvert x@(SqlInt64 _)          = quickError x
  safeConvert x@(SqlInteger _)        = quickError x
  safeConvert x@(SqlDouble _)         = quickError x
  safeConvert (SqlText x)             = parseTime' (iso8601DateFormat (Just "%T%Q")) $ TL.unpack x
  safeConvert x@(SqlBlob _)           = quickError x
  safeConvert x@(SqlBool _)           = quickError x
  safeConvert x@(SqlBitField _)       = quickError x
  safeConvert x@(SqlUUID _)           = quickError x
  safeConvert x@(SqlUTCTime _)        = quickError x -- converting UTC time to TimeOfDay has no sense, you must convert it to LocalTime explicitly giving TimeZone
  safeConvert (SqlLocalDate d)        = return $ LocalTime d midnight
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert (SqlLocalTime a)        = return a
  safeConvert y@SqlNull               = quickError y

instance Convertible UTCTime SqlValue where
    safeConvert = return . SqlUTCTime
instance Convertible SqlValue UTCTime where
  safeConvert x@(SqlDecimal _)        = quickError x -- converting number to UTC has no sense
  safeConvert x@(SqlInt32 _)          = quickError x
  safeConvert x@(SqlInt64 _)          = quickError x
  safeConvert x@(SqlInteger _)        = quickError x
  safeConvert x@(SqlDouble _)         = quickError x
  safeConvert (SqlText x)             = parseTime' (iso8601DateFormat (Just "%T%Q")) $ TL.unpack x
  safeConvert x@(SqlBlob _)           = quickError x
  safeConvert x@(SqlBool _)           = quickError x
  safeConvert x@(SqlBitField _)       = quickError x
  safeConvert x@(SqlUUID _)           = quickError x
  safeConvert (SqlUTCTime a)          = return a
  safeConvert x@(SqlLocalDate _)      = quickError x
  safeConvert x@(SqlLocalTimeOfDay _) = quickError x
  safeConvert x@(SqlLocalTime _)      = quickError x
  safeConvert y@SqlNull               = quickError y

instance (Convertible a SqlValue) => Convertible (Maybe a) SqlValue where
    safeConvert Nothing = return SqlNull
    safeConvert (Just a) = safeConvert a
instance (Convertible SqlValue a) => Convertible SqlValue (Maybe a) where
    safeConvert SqlNull = return Nothing
    safeConvert a = fmap Just $ safeConvert a

-- | Read a value from a string, and give an informative message
--   if it fails.
read' :: (Typeable a, Read a, Convertible SqlValue a) => TL.Text -> ConvertResult a
read' s =
    case [x | (x, t) <- reads rs, ("", "") <- lex t] of
      [x] -> Right x
      _ -> convError "Cannot read source value as dest type" (SqlText s)
  where
    rs = TL.unpack s

#ifdef __HUGS__
parseTime' :: (Typeable t, Convertible SqlValue t) => String -> String -> ConvertResult t
parseTime' _ inpstr =
    convError "Hugs does not support time parsing" (SqlString inpstr)
#else
parseTime' :: (Typeable t, Convertible SqlValue t, ParseTime t) => String -> String -> ConvertResult t
parseTime' fmtstr inpstr =
    case parseTime defaultTimeLocale fmtstr inpstr of
      Nothing -> convError ("Cannot parse using default format string " ++ show fmtstr)
                 (SqlText $ TL.pack inpstr)
      Just x -> Right x
#endif

-- | As the semantic of System.Locale.iso8601DateFormat has changed with
--   old-locale-1.0.0.2 in a non-compatible way, we now define our own
--   (compatible) version of it.
iso8601DateFormat :: Maybe String -> String
iso8601DateFormat mTimeFmt =
    "%Y-%m-%d" ++ case mTimeFmt of
             Nothing  -> ""
             Just fmt -> ' ' : fmt

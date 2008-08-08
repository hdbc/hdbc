module Database.HDBC.Statement
    (
    Statement(..),
    SqlError(..),
    SqlType(..), nToSql, iToSql,
    SqlValue(..)
    )

where
import Data.Dynamic
import qualified Data.ByteString as B
import Data.Char(ord,toUpper)
import Data.Word
import Data.Int
import System.Time
import Database.HDBC.ColTypes

data Statement = Statement
    {
     {- | Execute the prepared statement, passing in the given positional
        parameters (that should take the place of the question marks
        in the call to 'prepare').

        For non-SELECT queries, the return value is the number of
        rows modified, if known.  If no rows were modified, you get 0.
        If the value is unknown, you get -1.  All current HDBC drivers
        support this function and should never return -1.

        For SELECT queries, you will always get 0.

        This function should automatically call finish() to finish the previous
        execution, if necessary.
        -}
     execute :: [SqlValue] -> IO Integer,

     {- | Execute the query with many rows. 
        The return value is the return value from the final row 
        as if you had called 'execute' on it.

        Due to optimizations that are possible due to different
        databases and driver designs, this can often be significantly
        faster than using 'execute' multiple times since queries
        need to be compiled only once.

        This is most useful for non-SELECT statements. -}
     executeMany :: [[SqlValue]] -> IO (),
                 
     {- | Abort a query in progress -- usually not needed. -}
     finish :: IO (),

     {- | Fetches one row from the DB.  Returns 'Nothing' if there
        are no more rows.  Will automatically call 'finish' when
        the last row is read. -}
     fetchRow :: IO (Maybe [SqlValue]),

     {- | Returns a list of the column names in the result.
        For maximum portability, you should not assume that
        information is available until after an 'execute' function
        has been run.
        
        Information is returned here directly as returned
        by the underlying database layer.  Note that different
        databases have different rules about capitalization
        of return values and about representation of names
        of columns that are not simple columns.  For this reason,
        it is suggested that you treat this information for
        display purposes only.  Failing that, you should convert
        to lower (or upper) case, and use @AS@ clauses for
        anything other than simple columns.

        A simple getColumnNames implementation could simply
        apply @map fst@ to the return value of 'describeResult'.
        -}
     getColumnNames :: IO [String],


     {- | The original query that this 'Statement' was prepared
          with. -}
     originalQuery :: String,
     {- | Obtain information about the columns in the result set.
          Must be run only after 'execute'.  The String in the result
          set is the column name.

          You should expect this to be returned in the same manner
          as a result from 'Database.HDBC.fetchAllRows''.

          All results should be converted to lowercase for you
          before you see them.

          Please see caveats under 'getColumnNames' for information
          on the column name field here.
 -}
     describeResult :: IO [(String, SqlColDesc)]
    }

{- | The main HDBC exception object.  As much information as possible
is passed from the database through to the application through this object.

Errors generated in the Haskell layer will have seNativeError set to -1.
-}
data SqlError = SqlError {seState :: String,
                          seNativeError :: Int,
                          seErrorMsg :: String}
                deriving (Eq, Show, Read)

sqlErrorTc :: TyCon
sqlErrorTc = mkTyCon "Database.HDBC.SqlError"

instance Typeable SqlError where
    typeOf _ = mkTyConApp sqlErrorTc []

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
    toSql :: a -> SqlValue
    fromSql :: SqlValue -> a

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

The default representation of time values is an integer number of seconds.
Databases such as PostgreSQL with builtin timestamp types can will see
automatic conversion between these Haskell types to local types.  Other
databases can just use an int or a string. 

This behavior also exists for other types.  For instance, many databases don't
have a Rational type, so they'll just use Haskell's show function and
store a Rational as a string.

Two SqlValues are considered to be equal if one of these hold (first one that
is true holds; if none are true, they are not equal):
 * Both are NULL
 * Both represent the same type and the encapsulated values are equal
 * The values of each, when converted to a string, are equal. -}
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
              | SqlEpochTime Integer -- ^ Representation of ClockTime or CalendarTime
              | SqlTimeDiff Integer -- ^ Representation of TimeDiff
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
    SqlEpochTime a == SqlEpochTime b = a == b
    SqlTimeDiff a == SqlTimeDiff b = a == b
    SqlNull == SqlNull = True
    SqlNull == _ = False
    _ == SqlNull = False
    a == b = ((fromSql a)::String) == ((fromSql b)::String)

instance SqlType String where
    toSql = SqlString
    fromSql (SqlString x) = x
    fromSql (SqlByteString x) = byteString2String x
    fromSql (SqlInt32 x) = show x
    fromSql (SqlInt64 x) = show x
    fromSql (SqlWord32 x) = show x
    fromSql (SqlWord64 x) = show x
    fromSql (SqlInteger x) = show x
    fromSql (SqlChar x) = [x]
    fromSql (SqlBool x) = show x
    fromSql (SqlDouble x) = show x
    fromSql (SqlRational x) = show x
    fromSql (SqlEpochTime x) = show x
    fromSql (SqlTimeDiff x) = show x
    fromSql (SqlNull) = error "fromSql: cannot convert SqlNull to String"

instance SqlType B.ByteString where
    toSql = SqlByteString
    fromSql (SqlByteString x) = x
    fromSql (SqlNull) = error "fromSql: cannot convert SqlNull to ByteString"
    fromSql x = (string2ByteString . fromSql) x

string2ByteString :: String -> B.ByteString
string2ByteString = B.pack . map (toEnum . fromEnum)

instance SqlType Int where
    toSql x = SqlInt32 (fromIntegral x)
    fromSql (SqlString x) = read' x
    fromSql (SqlByteString x) = (read' . byteString2String) x
    fromSql (SqlInt32 x) = fromIntegral x
    fromSql (SqlInt64 x) = fromIntegral x
    fromSql (SqlWord32 x) = fromIntegral x
    fromSql (SqlWord64 x) = fromIntegral x
    fromSql (SqlInteger x) = fromIntegral x
    fromSql (SqlChar x) = ord x
    fromSql (SqlBool x) = if x then 1 else 0
    fromSql (SqlDouble x) = truncate $ x
    fromSql (SqlRational x) = truncate $ x
    fromSql (SqlEpochTime x) = fromIntegral x
    fromSql (SqlTimeDiff x) = fromIntegral x
    fromSql (SqlNull) = error "fromSql: cannot convert SqlNull to Int"

instance SqlType Int32 where
    toSql = SqlInt32
    fromSql (SqlString x) = read' x
    fromSql (SqlByteString x) = (read' . byteString2String) x
    fromSql (SqlInt32 x) = x
    fromSql (SqlInt64 x) = fromIntegral x
    fromSql (SqlWord32 x) = fromIntegral x
    fromSql (SqlWord64 x) = fromIntegral x
    fromSql (SqlInteger x) = fromIntegral x
    fromSql (SqlChar x) = fromIntegral $ ord x
    fromSql (SqlBool x) = if x then 1 else 0
    fromSql (SqlDouble x) = truncate $ x
    fromSql (SqlRational x) = truncate $ x
    fromSql (SqlEpochTime x) = fromIntegral x
    fromSql (SqlTimeDiff x) = fromIntegral x
    fromSql (SqlNull) = error "fromSql: cannot convert SqlNull to Int32"

instance SqlType Int64 where
    toSql = SqlInt64
    fromSql (SqlString x) = read' x
    fromSql (SqlByteString x) = (read' . byteString2String) x
    fromSql (SqlInt32 x) = fromIntegral x
    fromSql (SqlInt64 x) = x
    fromSql (SqlWord32 x) = fromIntegral x
    fromSql (SqlWord64 x) = fromIntegral x
    fromSql (SqlInteger x) = fromIntegral x
    fromSql (SqlChar x) = fromIntegral $ ord x
    fromSql (SqlBool x) = if x then 1 else 0
    fromSql (SqlDouble x) = truncate $ x
    fromSql (SqlRational x) = truncate $ x
    fromSql (SqlEpochTime x) = fromIntegral x
    fromSql (SqlTimeDiff x) = fromIntegral x
    fromSql (SqlNull) = error "fromSql: cannot convert SqlNull to Int64"

instance SqlType Word32 where
    toSql = SqlWord32
    fromSql (SqlString x) = read' x
    fromSql (SqlByteString x) = (read' . byteString2String) x
    fromSql (SqlInt32 x) = fromIntegral x
    fromSql (SqlInt64 x) = fromIntegral x
    fromSql (SqlWord32 x) = x
    fromSql (SqlWord64 x) = fromIntegral x
    fromSql (SqlInteger x) = fromIntegral x
    fromSql (SqlChar x) = fromIntegral $ ord x
    fromSql (SqlBool x) = if x then 1 else 0
    fromSql (SqlDouble x) = truncate $ x
    fromSql (SqlRational x) = truncate $ x
    fromSql (SqlEpochTime x) = fromIntegral x
    fromSql (SqlTimeDiff x) = fromIntegral x
    fromSql (SqlNull) = error "fromSql: cannot convert SqlNull to Word32"

instance SqlType Word64 where
    toSql = SqlWord64
    fromSql (SqlString x) = read' x
    fromSql (SqlByteString x) = (read' . byteString2String) x
    fromSql (SqlInt32 x) = fromIntegral x
    fromSql (SqlInt64 x) = fromIntegral x
    fromSql (SqlWord32 x) = fromIntegral x
    fromSql (SqlWord64 x) = x
    fromSql (SqlInteger x) = fromIntegral x
    fromSql (SqlChar x) = fromIntegral (ord x)
    fromSql (SqlBool x) = if x then 1 else 0
    fromSql (SqlDouble x) = truncate $ x
    fromSql (SqlRational x) = truncate $ x
    fromSql (SqlEpochTime x) = fromIntegral x
    fromSql (SqlTimeDiff x) = fromIntegral x
    fromSql (SqlNull) = error "fromSql: cannot convert SqlNull to Int64"

instance SqlType Integer where
    toSql = SqlInteger
    fromSql (SqlString x) = read' x
    fromSql (SqlByteString x) = (read' . byteString2String) x
    fromSql (SqlInt32 x) = fromIntegral x
    fromSql (SqlInt64 x) = fromIntegral x
    fromSql (SqlWord32 x) = fromIntegral x
    fromSql (SqlWord64 x) = fromIntegral x
    fromSql (SqlInteger x) = x
    fromSql (SqlChar x) = fromIntegral (ord x)
    fromSql (SqlBool x) = if x then 1 else 0
    fromSql (SqlDouble x) = truncate $ x
    fromSql (SqlRational x) = truncate $ x
    fromSql (SqlEpochTime x) = x
    fromSql (SqlTimeDiff x) = x
    fromSql (SqlNull) = error "fromSql: cannot convert SqlNull to Integer"

instance SqlType Bool where
    toSql = SqlBool
    fromSql (SqlString x) = 
        case map toUpper x of
                           "TRUE" -> True
                           "T" -> True
                           "FALSE" -> False
                           "F" -> False
                           "0" -> False
                           "1" -> True
                           _ -> error $ "fromSql: cannot convert SqlString " 
                                        ++ show x ++ " to Bool"
    fromSql (SqlByteString x) = (fromSql . SqlString . byteString2String) x
    fromSql (SqlInt32 x) = numToBool x
    fromSql (SqlInt64 x) = numToBool x
    fromSql (SqlWord32 x) = numToBool x
    fromSql (SqlWord64 x) = numToBool x
    fromSql (SqlInteger x) = numToBool x
    fromSql (SqlChar x) = numToBool (ord x)
    fromSql (SqlBool x) = x
    fromSql (SqlDouble x) = numToBool x
    fromSql (SqlRational x) = numToBool x
    fromSql (SqlEpochTime x) = numToBool x
    fromSql (SqlTimeDiff x) = numToBool x
    fromSql (SqlNull) = error "fromSql: cannot convert SqlNull to Bool"

numToBool :: Num a => a -> Bool
numToBool x = x /= 0

instance SqlType Char where
    toSql = SqlChar
    fromSql (SqlString [x]) = x
    fromSql (SqlByteString x) = (head . byteString2String) x
    fromSql (SqlString _) = error "fromSql: cannot convert SqlString to Char"
    fromSql (SqlInt32 _) = error "fromSql: cannot convert SqlInt32 to Char"
    fromSql (SqlInt64 _) = error "fromSql: cannot convert SqlInt64 to Char"
    fromSql (SqlWord32 _) = error "fromSql: cannot convert SqlWord32 to Char"
    fromSql (SqlWord64 _) = error "fromSql: cannot convert SqlWord64 to Char"
    fromSql (SqlInteger _) = error "fromSql: cannot convert SqlInt to Char"
    fromSql (SqlChar x) = x
    fromSql (SqlBool x) = if x then '1' else '0'
    fromSql (SqlDouble _) = error "fromSql: cannot convert SqlDouble to Char"
    fromSql (SqlRational _) = error "fromSql: cannot convert SqlRational to Char"
    fromSql (SqlEpochTime _) = error "fromSql: cannot convert SqlEpochTime to Char"
    fromSql (SqlTimeDiff _) = error "fromSql: cannot convert SqlTimeDiff to Char"
    fromSql (SqlNull) = error "fromSql: cannot convert SqlNull to Char"

instance SqlType Double where
    toSql = SqlDouble
    fromSql (SqlString x) = read' x
    fromSql (SqlByteString x) = (read' . byteString2String) x
    fromSql (SqlInt32 x) = fromIntegral x
    fromSql (SqlInt64 x) = fromIntegral x
    fromSql (SqlWord32 x) = fromIntegral x
    fromSql (SqlWord64 x) = fromIntegral x
    fromSql (SqlInteger x) = fromIntegral x
    fromSql (SqlChar x) = fromIntegral . ord $ x
    fromSql (SqlBool x) = if x then 1.0 else 0.0
    fromSql (SqlDouble x) = x
    fromSql (SqlRational x) = fromRational x
    fromSql (SqlEpochTime x) = fromIntegral x
    fromSql (SqlTimeDiff x) = fromIntegral x
    fromSql (SqlNull) = error "fromSql: cannot convert SqlNull to Double"

instance SqlType Rational where
    toSql = SqlRational
    fromSql (SqlString x) = read' x
    fromSql (SqlByteString x) = (read' . byteString2String) x
    fromSql (SqlInt32 x) = fromIntegral x
    fromSql (SqlInt64 x) = fromIntegral x
    fromSql (SqlWord32 x) = fromIntegral x
    fromSql (SqlWord64 x) = fromIntegral x
    fromSql (SqlInteger x) = fromIntegral x
    fromSql (SqlChar x) = fromIntegral . ord $ x
    fromSql (SqlBool x) = fromIntegral $ ((fromSql (SqlBool x))::Int)
    fromSql (SqlDouble x) = toRational x
    fromSql (SqlRational x) = x
    fromSql (SqlEpochTime x) = fromIntegral x
    fromSql (SqlTimeDiff x) = fromIntegral x
    fromSql (SqlNull) = error "fromSql: cannot convert SqlNull to Double"

instance SqlType ClockTime where
    toSql (TOD x _) = SqlEpochTime x
    fromSql (SqlString x) = TOD (read' x) 0
    fromSql (SqlByteString x) = TOD ((read' . byteString2String) x) 0
    fromSql (SqlInt32 x) = TOD (fromIntegral x) 0
    fromSql (SqlInt64 x) = TOD (fromIntegral x) 0
    fromSql (SqlWord32 x) = TOD (fromIntegral x) 0
    fromSql (SqlWord64 x) = TOD (fromIntegral x) 0
    fromSql (SqlInteger x) = TOD x 0
    fromSql (SqlChar _) = error "fromSql: cannot convert SqlChar to ClockTime"
    fromSql (SqlBool _) = error "fromSql: cannot convert SqlBool to ClockTime"
    fromSql (SqlDouble x) = TOD (truncate x) 0
    fromSql (SqlRational x) = TOD (truncate x) 0
    fromSql (SqlEpochTime x) = TOD x 0
    fromSql (SqlTimeDiff _) = error "fromSql: cannot convert SqlTimeDiff to ClockTime"
    fromSql SqlNull = error "fromSql: cannot convert SqlNull to ClockTime"

instance SqlType TimeDiff where
    toSql x = SqlTimeDiff (timeDiffToSecs x)
    fromSql (SqlString x) = secs2td (read' x)
    fromSql (SqlByteString x) = secs2td ((read' . byteString2String) x)
    fromSql (SqlInt32 x) = secs2td (fromIntegral x)
    fromSql (SqlInt64 x) = secs2td (fromIntegral x)
    fromSql (SqlWord32 x) = secs2td (fromIntegral x)
    fromSql (SqlWord64 x) = secs2td (fromIntegral x)
    fromSql (SqlInteger x) = secs2td x
    fromSql (SqlChar _) = error "fromSql: cannot convert SqlChar to TimeDiff"
    fromSql (SqlBool _) = error "fromSql: cannot convert SqlBool to TimeDiff"
    fromSql (SqlDouble x) = secs2td (truncate x)
    fromSql (SqlRational x) = secs2td (truncate x)
    fromSql (SqlEpochTime _) = error "fromSql: cannot convert SqlEpochTime to TimeDiff"
    fromSql (SqlTimeDiff x) = secs2td x
    fromSql SqlNull = error "fromSql: cannot convert SqlNull to TimeDiff"

instance SqlType CalendarTime where
    toSql x = toSql (toClockTime x)
    fromSql = toUTCTime . fromSql

instance (SqlType a) => SqlType (Maybe a) where
    toSql Nothing = SqlNull
    toSql (Just a) = toSql a
    fromSql SqlNull = Nothing
    fromSql x = Just (fromSql x)

byteString2String :: B.ByteString -> String
byteString2String = map (toEnum . fromEnum) . B.unpack

secs2td :: Integer -> TimeDiff
secs2td x = diffClockTimes (TOD x 0) (TOD 0 0)


-- | Read a value from a string, and give an informative message
--   if it fails.
read' :: (Typeable a,Read a) => String -> a
read' s = ret
  where ret = case reads s of
                  [(x,"")] -> x
                  _ -> error $ "fromSql: Cannot read " ++ show s 
                               ++ " as " ++ t ++ "."
        t = show (typeOf ret)

--------------
-- The following function copied from MissingH.Time.hs

{- | Converts the given timeDiff to the number of seconds it represents.

Uses the same algorithm as normalizeTimeDiff in GHC. -}
timeDiffToSecs :: TimeDiff -> Integer
timeDiffToSecs td =
    (fromIntegral $ tdSec td) +
    60 * ((fromIntegral $ tdMin td) +
          60 * ((fromIntegral $ tdHour td) +
                24 * ((fromIntegral $ tdDay td) +
                      30 * ((fromIntegral $ tdMonth td) +
                            365 * (fromIntegral $ tdYear td)))))

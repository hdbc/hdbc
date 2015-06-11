{-# LANGUAGE DeriveDataTypeable #-}
module Database.HDBC.Statement
    (
    Statement(..),
    SqlError(..),

    -- * Re-exported from "Database.HDBC.SqlValue"; this re-exporting is deprecated
    nToSql, iToSql, posixToSql, fromSql, safeFromSql, toSql,
    SqlValue(..)
    )

where
import Data.Dynamic
import Database.HDBC.ColTypes
import Database.HDBC.SqlValue
import Control.Exception

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

     {- | Execute the statement as-is, without supplying any
        positional parameters.  This is intended for statements for
        which the results aren't interesting or present (e.g., DDL or
        DML commands).  If your query contains placeholders, this will
        certainly fail; use 'execute' instead. -}
     executeRaw :: IO (),

     {- | Execute the query with many rows. 
        The return value is the return value from the final row 
        as if you had called 'execute' on it.

        Due to optimizations that are possible due to different
        databases and driver designs, this can often be significantly
        faster than using 'execute' multiple times since queries
        need to be compiled only once.

        This is most useful for non-SELECT statements. -}
     executeMany :: [[SqlValue]] -> IO (),

     {- | Abort a query in progress -- usually not needed. This allows
        executing the query once again later with a different parameter set -}
     finish :: IO (),

     {- | Finalizes the statement and immediately frees up any associated resources.
          Doing anything with a finalized statement will break your program.

          This is mainly intended for optimization to avoid waiting for GC to invoke
          finalizer on the statement. -}
     finalize :: IO (),

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
                deriving (Eq, Show, Read, Typeable)


#if __GLASGOW_HASKELL__ >= 610
--data SqlException
instance Exception SqlError where
{-
    toException = SomeException
    fromException (SomeException e) = Just e
    fromException _ = Nothing
-}
#endif

{-
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and\/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

-}

{- |
   Module     : Database.HDBC.Types
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU LGPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Types for HDBC.

Please note: this module is intended for authors of database driver libraries
only.  Authors of applications using HDBC should use 'Database.HDBC'
exclusively.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Database.HDBC.Types
    (Connection(..),
     Statement(..),
     SqlError(..)

    )

where
import Data.Dynamic

{- | Main database handle object.

This object must be created by a driver for a specific database (see
the database driver modules).

A call to 'commit' is required to make sure that your changes get committed
to the database.
-}
data Connection = 
    Connection {
                {- | Disconnect from the remote database.

You do not need to explicitly close a Connection object, but you may do so if
you so desire.  If you don't, the object will disconnect from the database
in a sane way when it is garbage-collected.  However, a disconnection may
raise an error, so you are encouraged to explicitly call 'disconnect'.  Also,
garbage collection may not run when the program terminates, and some databases
really like an explicit disconnect.

This function discards any data not committed already.  Database driver
implementators should explicitly call 'rollback' if their databases don't
do this automatically on disconnect.

Bad Things (TM) could happen if you call this while you have Statements active.

-}
                disconnect :: IO (),
                {- | Commit any pending data to the database.

                   Required to make any changes take effect. -}
                commit :: IO (),
                {- | Roll back to the state the database was in prior to the
                   last 'commit' or 'rollback'. -}
                rollback :: IO (),
                {- | Execute a single SQL query.  Returns the number
                   of rows modified.  The second parameter is a list
                   of replacement strings, if any. -}
                run :: String -> [Maybe String] -> IO Integer,
                {- | Prepares a statement for execution. 

                   Question marks in the statement will be replaced by
                   positional parameters in a later call to 'sExecute'.

                   Please note that, depending on the database
                   and the driver, errors in your SQL may be raised
                   either here or by 'sExecute'.  Make sure you
                   handle exceptions both places if necessary. -}
                prepare :: String -> IO Statement
               }

data Statement = Statement
    {
     {- | Execute the prepared statement, passing in the given positional
        parameters (that should take the place of the question marks
        in the call to 'prepare').  Note that not all databases may
        be able to return a row count immediately; those that can't
        will return -1.  Even those that can may be inaccurate.  Use
        this value with a grain of salt.

        This function should call finish() to finish the previous
        execution, if necessary.  (This should be necessary when
        'isActive' is True.)
        -}
     sExecute :: [Maybe String] -> IO Integer,

     {- | Execute the query with many rows. 
        The return value is the return value from the final row 
        as if you had called 'sExecute' on it.

        Due to optimizations that are possible due to different
        databases and driver designs, this can often be significantly
        faster than using 'sExecute' multiple times since queries
        need to be compiled only once. -}
     sExecuteMany :: [[Maybe String]] -> IO Integer,
     {-  Returns true if a query is in progress.
         
         In general, whenever this is True, the driver should know to
         implicitly call 'finish' when the next 'sExecute' is called.

      Usually, this should be False when
      the Statement has just been created, True after 'sExecute' is run,
      and then False after the last row is read (or after 'sExecute' returns
      no data.)  However, this guideline may vary from driver to driver,
      especially the behavior after initial creation.

      'finish' should always flip this flag to False (unless 'finish'
      dies with an exception).  -}
     --isActive :: IO Bool,

     {-  True if a statement has been executed.  Flips back to False
          when 'finish' is caled.
     -}
     -- isExecuted :: IO Bool,
                 
     {- | Abort a query in progress -- usually not needed. -}
     finish :: IO (),

     {- | Fetches one row from the DB.  Returns 'Nothing' if there
        are no more rows.  Will automatically call 'finish' when
        the last row is read. -}
     fetchRow :: IO (Maybe [Maybe String])
    }

-- stripped down version of HSQL item
data SqlError = SqlError {seState :: String,
                          seNativeError :: Int,
                          seErrorMsg :: String}
                deriving (Eq, Show, Read, Typeable)

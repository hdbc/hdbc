{-
Copyright (C) 2005-2007 John Goerzen <jgoerzen@complete.org>

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
   Module     : Database.HDBC.Types
   Copyright  : Copyright (C) 2005-2007 John Goerzen
   License    : GNU LGPL, version 2.1 or above

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
    (IConnection(..),
    Statement(..),
    SqlError(..),
    SqlType(..), nToSql, iToSql,
    SqlValue(..),
    ConnWrapper(..),
    withWConn
    )

where
import Database.HDBC.Statement
import Database.HDBC.ColTypes

{- | Main database handle object.

An 'IConnection' object is created by specific functions in the module for an
individual database.  That is, the connect function -- which creates
this object -- is not standardized through the HDBC interface.

A connection is closed by a call to 'disconnect'.

A call to 'commit' is required to make sure that your changes get committed
to the database.  In other words, HDBC has /no support for autocommit/, which
we consider an outdated notion.
-}
class IConnection conn where
                {- | Disconnect from the remote database.

You do not need to explicitly close an IConnection object, but you may do so if
you so desire.  If you don't, the object will disconnect from the database
in a sane way when it is garbage-collected.  However, a disconnection may
raise an error, so you are encouraged to explicitly call 'disconnect'.  Also,
garbage collection may not run when the program terminates, and some databases
really like an explicit disconnect.

So, bottom line is, you're best off calling 'disconnect' directly, but the
world won't end if you forget.

This function discards any data not committed already.  Database driver
implementators should explicitly call 'rollback' if their databases don't
do this automatically on disconnect.

Bad Things (TM) could happen if you call this while you have 'Statement's 
active.  In more precise language, the results in such situations are undefined
and vary by database.  So don't do it.

-}
                disconnect :: conn -> IO ()
                {- | Commit any pending data to the database.

                   Required to make any changes take effect. -}
                commit :: conn -> IO ()
                {- | Roll back to the state the database was in prior to the
                   last 'commit' or 'rollback'. -}
                rollback :: conn -> IO ()
                {- | Execute a single SQL query.  Returns the number
                   of rows modified (see 'execute' for details).
                   The second parameter is a list
                   of replacement values, if any. -}
                run :: conn -> String -> [SqlValue] -> IO Integer
                {- | Prepares a statement for execution. 

                   Question marks in the statement will be replaced by
                   positional parameters in a later call to 'execute'.

                   Please note that, depending on the database
                   and the driver, errors in your SQL may be raised
                   either here or by 'execute'.  Make sure you
                   handle exceptions both places if necessary. -}
                prepare :: conn -> String -> IO Statement
                {- | Create a new 'Connection' object, pointed at the same
                   server as this object is.  This will generally establish
                   a separate physical connection.

                   When you wish to establish multiple connections to a single
                   server, the correct way to do so is to establish the
                   first connection with the driver-specific connection
                   function, and then clone it for each additional connection.
                   
                   This can be important when a database doesn't provide
                   much thread support itself, and the HDBC driver module
                   must serialize access to a particular database.

                   This can also be a handy utility function whenever you
                   need a separate connection to whatever database you are
                   connected to already. -}
                clone :: conn -> IO conn


                {- | The name of the HDBC driver module for this connection.
                   Ideally would be the same as the database name portion
                   of the Cabal package name.  For instance, \"sqlite3\"
                   or \"odbc\".  This is the layer that is bound most
                   tightly to HDBC. -}
                hdbcDriverName :: conn -> String
                {- | The version of the C (or whatever) client library
                   that the HDBC driver module is bound to.  The meaning
                   of this is driver-specific.  For an ODBC or similar
                   proxying driver, this should be the version of the
                   ODBC library, not the eventual DB client driver. -}
                hdbcClientVer :: conn -> String
                {- | In the case of a system such as ODBC, the name of
                   the database client\/server in use, if available.
                   For others,
                   identical to 'hdbcDriverName'. -}
                proxiedClientName :: conn -> String
                {- | In the case of a system such as ODBC, the version of
                   the database client in use, if available.  For others,
                   identical to 'hdbcClientVer'. This is the next layer
                   out past the HDBC driver. -}
                proxiedClientVer :: conn -> String
                {- | The version of the database server, if available. -}
                dbServerVer :: conn -> String
                {- | Whether or not the current database supports transactions.
                   If False, then 'commit' and 'rollback' should be expected
                   to raise errors.

                   MySQL is the only commonly-used database that is known
                   to not support transactions entirely.  Please see
                   the MySQL notes in the ODBC driver for more information. -}
                dbTransactionSupport :: conn -> Bool

                {- | The names of all tables accessible by the current
                   connection, excluding special meta-tables (system tables).
                   
                   You should expect this to be returned in the same manner
                   as a result from 'Database.HDBC.fetchAllRows''.

                   All results should be converted to lowercase for you
                   before you see them.
                     -}
                getTables :: conn -> IO [String]

                {- | Obtain information about the columns in a specific
                   table.  The String in the result
                   set is the column name.

                   You should expect this to be returned in the same manner
                   as a result from 'Database.HDBC.fetchAllRows''.

                   All results should be converted to lowercase for you
                   before you see them.
                   -}
                describeTable :: conn -> String -> IO [(String, SqlColDesc)]

{- | Sometimes, it is annoying to use typeclasses with Haskell's type system.
In those situations, you can use a ConnWrapper.  You can create one with:

>let wrapped = ConnWrapper iconn

You can then use this directly, since a ConnWrapper is also an
'IConnection'.  However, you will not be able to use private database
functions on it.

Or, you can use 'withWConn'.
-}
data ConnWrapper = forall conn. IConnection conn => ConnWrapper conn

{- | Unwrap a 'ConnWrapper' and pass the embedded 'IConnection' to
a function.  Example:

>withWConn wrapped run $ "SELECT * from foo where bar = 1" []
-}
withWConn :: forall b. ConnWrapper -> (forall conn. IConnection conn => conn -> b) -> b
withWConn conn f =
    case conn of
         ConnWrapper x -> f x

instance IConnection ConnWrapper where
    disconnect w = withWConn w disconnect
    commit w = withWConn w commit
    rollback w = withWConn w rollback
    run w = withWConn w run
    prepare w = withWConn w prepare
    clone w = withWConn w (\dbh -> clone dbh >>= return . ConnWrapper)
    hdbcDriverName w = withWConn w hdbcDriverName
    hdbcClientVer w = withWConn w hdbcClientVer
    proxiedClientName w = withWConn w proxiedClientName
    proxiedClientVer w = withWConn w proxiedClientVer
    dbServerVer w = withWConn w dbServerVer
    dbTransactionSupport w = withWConn w dbTransactionSupport
    getTables w = withWConn w getTables
    describeTable w = withWConn w describeTable


{-
Copyright (C) 2005-2006 John Goerzen <jgoerzen@complete.org>

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
   Module     : Database.HDBC
   Copyright  : Copyright (C) 2005-2007 John Goerzen
   License    : GNU LGPL, version 2.1 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Welcome to HDBC, the Haskell Database Connectivity library.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Database.HDBC 
    (-- * Introduction
     -- $introduction

     -- ** Features
     -- $features

     -- ** Available Drivers
     -- $drivers

     -- * Typing of transfer data
     SqlType(..), nToSql, iToSql,
     SqlValue(..),

     -- * Database Connections
     IConnection,
     disconnect, clone,
     -- ** Wrapped Connections
     ConnWrapper(..), withWConn,
     -- ** Preparing Queries
     run, sRun, prepare, quickQuery', quickQuery,

     -- ** Transaction Handling
     -- $transactions
     commit, rollback, withTransaction,

     -- ** Connection Inquiries
     hdbcDriverName, hdbcClientVer, proxiedClientName,
     proxiedClientVer, dbServerVer, dbTransactionSupport,
     getTables, describeTable,

     -- * Statements
     Statement,
     -- ** Execution
     execute, sExecute, executeMany, sExecuteMany,
     -- ** Fetching Results
     fetchRow, fetchRowAL, fetchRowMap, sFetchRow, 
     fetchAllRows, fetchAllRows', fetchAllRowsAL, fetchAllRowsAL',
     fetchAllRowsMap, fetchAllRowsMap', sFetchAllRows, sFetchAllRows',
     getColumnNames,
     -- ** Statement Inquires
     describeResult,
     -- ** Miscellaneous
     finish, originalQuery,

     -- * Exceptions
     SqlError(..),
     catchSql, handleSql, sqlExceptions, handleSqlError,

     -- * Column Types
     -- | These are defined in "Database.HDBC.ColTypes" but are
     -- available to programs importing "Database.HDBC" by default as well.
     -- See "Database.HDBC.ColTypes" for documentation.
     module Database.HDBC.ColTypes

     -- * Threading
     -- $threading

     -- * Copyright and License
     -- $legal
    )

where
import Database.HDBC.Utils(catchSql, handleSql, sqlExceptions,
                           handleSqlError, withTransaction,
                           sFetchAllRows, fetchAllRows,
                           sFetchAllRows', fetchAllRows',
                           sRun, sExecute, sExecuteMany, sFetchRow,
                           quickQuery, fetchRowMap, fetchAllRowsMap,
                           quickQuery', fetchAllRowsMap',
                           fetchRowAL, fetchAllRowsAL,
                           fetchAllRowsAL')
import Database.HDBC.Types
import Database.HDBC.ColTypes

{- $introduction

Welcome to HDBC, Haskell Database Connectivity.

HDBC provides an abstraction layer between Haskell programs and SQL
relational databases.  This lets you write database code once, in
Haskell, and have it work with any number of backend SQL databases
(MySQL, Oracle, PostgreSQL, ODBC-compliant databases, etc.)

HDBC is modeled loosely on Perl's DBI interface
<http://search.cpan.org/~timb/DBI/DBI.pm>, though it has also
been influenced by Python's DB-API v2, JDBC in Java, and HSQL in
Haskell.

HDBC is a from-scratch effort.  It is not a reimplementation of HSQL,
though its purpose is the same.
-}

{- $features

Features of HDBC include:

 * Ability to use replacable parameters to let one query be
   executed multiple times (eliminates the need for an escape
   function)

 * Ability to access returned rows by column number

 * Ability to read data from the SQL server on-demand rather than
   reading the entire result set up front

 * HUnit testsuite for each backend driver

 * Well-defined standard API and easy backend driver implementation

 * Lazy reading of the entire result set (think hGetContents, but
   for the results of SELECT) (see 'sFetchAllRows')

 * Support for translation between Haskell and SQL types

 * Support for querying database server properties

 * Add-on package (hdbc-missingh) to integrate with MissingH,
   providing a database backend for AnyDBM.

 * Support for querying metadata such as column names.

 * Support for querying additional metadata (column types, etc.)
-}

{- $drivers

Here is a list of known drivers as of March 28, 2006:

[@Sqlite v3@] Available from <http://software.complete.org/hdbc-sqlite3>.  Or, to
participate in development, use 
@darcs get --partial <http://darcs.complete.org/hdbc-sqlite3>@

[@PostgreSQL@] Available from <http://software.complete.org/hdbc-postgresql>.  Or, to
participate in development, use
@darcs get --partial <http://darcs.complete.org/hdbc-postgresql>@

[@ODBC@] Available from <http://software.complete.org/hdbc-odbc>.  Or, to
partitipace in development, use
@darcs get --partial <http://darcs.complete.org/hdbc-odbc>@

[@MySQL@] MySQL users are encouraged to use the ODBC driver, which works
and has been tested against MySQL on both Linux\/Unix and Windows platforms.

In addition, there is one integration package: /hdbc-anydbm/.  This
integrates with the AnyDBM library <http://software.complete.org/anydbm>.
It lets any HDBC database act as a backend for the
AnyDBM interface.  Available from <http://software.complete.org/hdbc-anydbm>.  Or,
to participate in development, use
@darcs get --partial <http://darcs.complete.org/hdbc-anydbm>@

The latest version of HDBC itself is available from
<http://software.complete.org/hdbc>.  Or, to participate in development, use
@darcs get --partial <http://darcs.complete.org/hdbc>@.
-}

{- $transactions

This section concerns itself with writing (updating) a database.

In HDBC, as with many RDBMS implementations, every write to the
database occurs within a transaction.  No changes are visible (outside
the current transaction) until a commit operation occurs, in which
case all changes since the transaction started are atomically
committed.  Also, there is a rollback operation that can undo all
changes since the transaction started.

HDBC does everything within a transaction.  A transaction is implicitly entered
when a connection to a database is established, and a transaction is
implicitly entered after each call to 'commit' or 'rollback' as well.

The practical effect of this is that you must call 'commit' after making
changes to a database in order for those changes to become visible.  You don't
have to call 'commit' after /every/ change, just after a batch of them.

(Exceptions exist for databases that don't offer a high level of transaction
isolation; but you should always play it safe and commit anyway.)

Database developers will also be experienced with the atomicity benefits
of transactions, an explanation of which is outside the scope of this manual.

Errors occuring at the database level can leave a transaction in an
indeterminate state, depending on the database.  Some databases will
refuse all queries until the next 'commit' or 'rollback'.  The safe thing
to do is to issue a 'commit' or 'rollback' after trapping any 'SqlError'.
Alternatively, you could use 'withTransaction', which will automatically
handle this detail for you.
-}

{- $threading

FIXME: this is draft information

Thread support in a generalized interface such as HDBC can be complicated
because support for threading varies across database interfaces.

However, applications using HDBC should be able to rely upon at least a few
basic guarantees:

 * The HDBC modules may freely be imported and used across all threads.

 * HDBC modules may also freely share database connections and statements;
   the database or HDBC driver will be responsible for locking if necessary.

I use \"share\" in the same sense as Python's DB-API: multiple threads may use
the resource without wrapping it in any lock.

However, there are some caveats to the above:

 * Not all databases support more than one active statement for a single
   connection.  Therefore, for maximum portability, you should use
   a different connection to the database for each simultaneous query you
   wish to use.
   FIXME: describe when a statement is active.

 * Not all databases may support the level of multithreading described above.
   For those that don't, safe access will be restriced in the HDBC driver
   by using locks.  Therefore, you can write portable code, but you 
   only get real multithreading when databases really support it.
   Details of thread support should be documented in the HDBC
   driver for each specific database.
-}

{- $legal
Copyright (C) 2005-2007 John Goerzen <jgoerzen@complete.org>

This library is free software; you can redistribute it and\/or
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

Please see COPYING in the source distribution for the full license.
-}

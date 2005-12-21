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
   Module     : Database.HDBC
   Copyright  : Copyright (C) 2005 John Goerzen
   License    : GNU LGPL, version 2 or above

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

     -- * Database Connections
     Connection,
     -- ** Preparing Queries
     sRun, prepare,

     -- ** Transaction Handling
     -- $transactions
     commit, rollback, withTransaction,

     -- ** Miscellaneous
     disconnect, {- clone, -}
     -- * Statements
     Statement,
     -- ** Execution
     sExecute, sExecuteMany,
     -- ** Fetching Results
     sFetchRow, sFetchAllRows,
     -- ** Miscellaneous
     finish,
     -- * Exceptions
     SqlError(..),
     catchSql, handleSql, sqlExceptions, handleSqlError
    )

where
import Database.HDBC.Utils(catchSql, handleSql, sqlExceptions,
                           handleSqlError, withTransaction,
                           sFetchAllRows)
import Database.HDBC.Types

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

Features on the TODO list which will appear shortly include:

 * Support for translation between Haskell and SQL types

 * Lazy reading of the entire result set (think hGetContents, but
   for the results of SELECT)

 * Support for querying metadata (column names, types, etc.)

 * Additional infrastructure for querying database server properties

 * Support for translation between Haskell and SQL types

 * Lazy reading of the entire result set (think hGetContents, but
   for the results of SELECT)

 * Support for querying metadata (column names, types, etc.)

 * Additional infrastructure for querying database server properties

 * Add-on package to integrate with MissingH (filesystem in a
   database, backend for AnyDBM, etc.)
-}

{- $drivers

Here is a list of known drivers as of December 21, 2005:

 [@Sqlite v3@] Use @darcs get --partial <http://darcs.complete.org/hsql-sqlite3>@

-}

{- $transactions

This section concerns itself with writing (updating) a database.

In HDBC, as with many RDBMS implementations, every write to the database occurs
within a transaction.  No changes are visible until a commit operation
occurs, in which case all changes since the transaction started are atomically
committed.  Also, there is a rollback operation that can undo all changes
since the transaction started.

HDBC does everything within a transaction.  A transaction is implicitly entered
when a connection to a database is established, and a transaction is
implicitly entered after each call to 'commit' or 'rollback' as well.

The practical effect of this is that you must call 'commit' after making
changes to a database in order for those changes to become visible.  You don't
have to call 'commit' after /every/ change, just after a batch of them.

Database developers will also be experienced with the atomicity benefits
of transactions, an explanation of which is outside the scope of this manual.

-}
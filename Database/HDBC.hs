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

     -- * Database Handles
     Connection,
     disconnect, run, prepare, commit, rollback, 
     -- * Statements
     Statement,
     -- ** Execution
     sExecute, sExecuteMany,
     -- ** Fetching Results
     fetchRow,
     -- ** Miscellaneous
     finish,
     -- * Exceptions
     SqlError(..),
     catchSql, handleSql, sqlExceptions, handleSqlError
    )

where
import Database.HDBC.Utils
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

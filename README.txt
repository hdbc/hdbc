Welcome to HDBC, Haskell Database Connectivity.

HDBC is modeled loosely on Perl's DBI interface, though it has also
been influenced by Python's DB-API v2, JDBC in Java, and HSQL in
Haskell.

Features of HDBC
----------------

HDBC provides an abstraction layer between Haskell programs and SQL
relational databases.  This lets you write database code once, in
Haskell, and have it work with any number of backend SQL databases
(MySQL, Oracle, PostgreSQL, ODBC-compliant databases, etc.)

HDBC is a from-scratch effort.  It is not a reimplementation of HSQL,
though its purpose is the same.  Some features HDBC has which HSQL
lacks include:

 * Ability to use replacable parameters to let one query be
   executed multiple times (eliminates the need for an "escape"
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

 * Add-on package to integrate with MissingH (filesystem in a
   database, backend for AnyDBM, etc.)

HDBC Drivers
------------

The following HDBC drivers exist:

Sqlite v3, darcs get --partial http://darcs.complete.org/hdbc-sqlite3

More will be coming shortly.

-- John Goerzen
   December 20, 2005
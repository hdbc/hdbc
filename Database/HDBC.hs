{- |
   Module     : Database.HDBC
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable
 -}

module Database.HDBC
       (

-- | Welcome to HDBC, the Haskell Database Connectivity library.
-- Written by John Goerzen, jgoerzen\@complete.org

-- * Introduction

-- |HDBC provides an abstraction layer between Haskell programs and SQL
-- relational databases.  This lets you write database code once, in Haskell,
-- and have it work with any number of backend SQL databases (MySQL, Oracle,
-- PostgreSQL, ODBC-compliant databases, etc.)

-- * Difference between HDBC-2 and HDBC-3.

-- |This is the rewritten HDBC with new features and better design. Here is the
-- difference between HDBC-2 and HDBC-3:
--
--  * typeclass IConnection renamed to 'Connection' to be more Haskell-specific
--
--  * removed methods getTables and describeTable because they are not
--    compatible with project's goals.
--
--  * 'Statement' is not data but typeclass for now
--
--  * 'Connection' and 'Statement' instances must be 'Typeable' instances as
--    well. 'Typeable' let you to write database-independent code with
--    'ConnWrapper' and 'StmtWrapper' wrappers.
--
--  * 'ConnWrapper' and 'StmtWrapper' wrappers which can be downcasted to
--    specific 'Connection' or 'Statement' instance or used
--    directly. 'ConnWrapper' and 'StmtWrapper' are 'Connection' and 'Statement'
--    instances too
--
--  * 'SqlValue' constructors set is reduced.
--
--  * Removed any lazy IO operations.

-- * Goals of the project

-- |* Provide common interface to execute queries and send/fetch data to/from
-- the database.
--
--  * Provide the type (''SqlValue'') to represent data which can be sent or fetched
--    from the database.
--
--  * Provide convenient way to convert Haskell-side data to database-side data
--  and vice versa.
--
--  * Give the most wide set of supported types, such as Decimal (arbitrary
--    precision values), Integer, Date and Time
--
--  * Authomatic finalization of statements related to connection when
--  connection is closed.
--
--  * Thread safety.
--
--  * To be clean, minimalistic, well-tested and correct base for other
--  higher-level packages.

-- * Not goals of the project

-- | * Database introspection: HDBC must not know how to introspect the
--    database. This problem must be solved with separate package
--    e.g. hdbc-introspect providing the common interface to introspect the
--    schema and drivers hdbc-introspect-postgresql, hdbc-introspect-mysql and
--    so on to provide database-specific implementation of this
--    interface. Current HDBC architecture allows to acces to low-levl
--    connection and statement functions. The mandatory Typeable instance for
--    any Connection and Statement instance allows to downcast any polymorphic
--    type to specific type and do whatever you need.
--
--  * Implicit transactions: if you want to work in transaction you must use
--    withTransaction function which correctly rollback the transaction on
--    exceptions.
--
--  * Lazy IO and resource management: to be short Conduit and ResourceT
--
--  * Any other things not related to query execution.

-- * Differences in SqlValue

-- |New 'SqlValue' has just the munimum set of posible types which can be stored
-- on database side or fetched from the database.
--
--  * SqlString is replaced with SqlText which stores lazy text instead of
--    String. String is ineffective in memory and speed.
--
--  * SqlByteString is renamed to SqlBlob to be more verbal.
--
--  * SqlWord32 and SqlWord64 is removed because they have the same type on
--    database side as Int. E.g. there is no unsigned integer type in
--    PostgreSql.
--
--  * SqlChar is removed because this is the same as Text with one character,
--    and there is no special ''one char'' type.
--
--  * SqlRational is replaced with SqlDecimal because there is no one database
--    which has native Rational support. You can not to save any Rational value
--    to database and get back just the same. Decimal is more proper type to
--    represent database-level arbitrary precision value.
--
--  * SqlZonedLocalTimeOfDay is removed because there is no native support of
--    this type on database-level except the PostgreSQL. But the documentation
--    says, that this type is deprecated, difficult to use and must not be used
--    in new applications.
--
--  * SqlZonedTime, SqlPOSIXTime and SqlEpochTime are removed. They has
--    absolutely the same type on database side as SqlUTCTime. You can convert
--    PosixTime to UTCTime and vice versa using the instances from `convertible`
--    package, so there is no need in this consturctors. No one database has
--    native type storing ZoneInfo directly, every database convert zoned
--    datetime to utc format and apply local server's timezone to convert utc
--    back to zoned datetime when you select this value. So SqlZonedTime is just
--    the same as SqlUTCTime on database side.
--
--  * SqlZonedTime and SqlDiffTime removed because no wide support of this types on
--    database level. In fact just PostgreSql. But maybe I am wrong.

-- * Drivers

-- |Drivers must be implemented using clean and safe bindings. You must not use
-- C-hacks to interact with database client library, this is the binding's goal.
--
--  * HDBC-postgresql: use postgresql-libpq and postgresql-simple
--    bindings. PostgreSQL use @$n@ (where @n@ is parameter index) placeholder
--    for query parameters, but you can safely use @?@ placeholder like in other
--    databases. HDBC-postgresql replaces @?@ with sequential @$n@ placeholders
--    before passing the query to database. You can also use @$n@ directly but
--    will break portablility.

-- * Thread-safety

-- |All HDBC drivers must use thread safe MVars to store data, which can be
-- shared between threads.

-- * Reimported modules
         module Database.HDBC.SqlValue
       , module Database.HDBC.Types
       ) where


import Database.HDBC.SqlValue
import Database.HDBC.Types

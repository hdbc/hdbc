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

-- * Design notes

-- | There is two typeclasses 'Connection' and 'Statement'. Each database driver
-- must provide it's own types (e.g. PostgreConnection and PostgreStatement in
-- HDBC-postgresql driver) and instances for them. Driver can provide additional
-- low-level functions not covered by these typeclasses.
--
-- There is also database-independent wrappers 'StmtWrapper' and 'ConnWrapper'
-- which are instances of 'Statement' and 'Connection' respectively. These
-- wrappers can hold database-specific type and call it's instance methods to
-- interact with database. You can still use low-level functions provided for
-- wrapped type by casting the wrapper back to the specific type by functions
-- 'castStatement' and 'castConnection'. Here is how it's look like:
--
-- @
--genericStmtLen :: StmtWrapper -> IO Int
--genericStmtLen stmt = do
--   executeRaw stmt
--   case castStatement stmt of
--     Nothing -> wrapperLen stmt
--     Just pgstmt -> pgLen pgstmt
--
--wrapperLen :: StmtWrapper -> IO Int
--wrapperLen stmt = length \<$\> fetchAllRows stmt
--
--pgLen :: PostgreStatement -> IO Int
--pgLen = undefined -- assume implementation is provided by driver, the fast
--                  -- PostgreSQL-specific way by ''ntuples'' low-level function. This
--                  -- feature is not included to 'Statement' typeclass because some
--                  -- databases have no such posibility.
-- @
--
-- You can see, that 'StmtWrapper' is still can be converted safely to
-- PostgreStatement, and if you pass PostgreStatement wrapped in StmtWrapper
-- function ''genericStmtLen'' will automatically use fast PostgreSQL
-- implementation. If it could not cast statement to specific type it will use
-- slow implementation based on 'Statement' methods.
--
-- There is also 'SqlValue' representing value from database's columns. This
-- type has a set of constructors each of them holds value of some type. There
-- is 'SqlUTCTime' for saving and fetching 'UTCTime' value from the database,
-- and 'SqlText' to save and get the text from database fields. You can execute
-- queries like this:
--
-- @
-- {-# LANGUAGE
--   OverloadedStrings
--  #-}
--
--module Main where
--
--import Database.HDBC
--import Database.HDBC.PostgreSQL
--import Control.Applicative
--import Data.Time
--import qualified Data.ByteString as B
--import qualified Data.Text.Lazy as TL
--
--insertTuples :: (Connection conn) => [(Int, TL.Text, UTCTime, B.ByteString)] -> conn -> IO ()
--insertTuples x conn = runMany conn
--                      \"insert into tupletbl (ifield, tfield, dtfield, bsfield) values (?,?,?,?)\"
--                      $ map (\(a, b, c, d) -> [ toSql a , toSql b , toSql c , toSql d]) x
-- @
--
-- Note, that you can still safely use ''?'' parameter placeholders inside the
-- query even with PostgreSQL, which use ''$1'' like placeholders.
--
-- There is also 'Query' type which is type-safe newtype wrapper around simple
-- lazy Text. You can write it literal, using ''-XOverloadedStrings'' extension.
--
-- We do not use String except inside 'SqlError', not 'Query' nor 'SqlText' does
-- not use String inside.

-- * Some kind of roadmap

-- |* Finish other hdbc drivers, like mysql and sqlite
--
-- * Unify the testing and benchmarking with one package.
--
-- * Create package hdbc-introspect with common interface to introspect and
-- change the schema. Also it will be necessary to create packages
-- hdbc-introspect-postgresql, hdbc-introspect-mysql and so on for each database
-- using specific methods to introspect and change the schema. This is the base
-- package for doing migrations like in Ruby on Rails.
--
-- * Create hdbc-resourcet and hdbc-conduit to provide convenient and reliable
-- way for finalizing statements and streaming processing the query results.
--
-- * Port other high-level database interfaces, like ''persistent'' and
-- ''haskelldb''. This will posibly lead to need to patch ''persistent'' and/or
-- ''haskelldb'' to support Decimal for example, or not. This is too earnly to
-- plan this in detail.

-- * Project scope

-- |* Provide common interface to execute queries and send/fetch data to/from
-- the database.
--
--  * Provide the type (''SqlValue'') to represent data which can be sent or fetched
--    from the database.
--
--  * Provide convenient way to convert Haskell-side data to database-side data
--  and vice versa. This is done with 'Convertible' instances.
--
--  * Give the most wide set of supported types, such as Decimal (arbitrary
--    precision values), Integer, Date and Time
--
--  * Safe interface with protection from re-release of resources. There must
--  not be errors, the more segafults, if you try to finish the statement when
--  the connection is already closed.
--
--  * Thread-safety.
--
--  * Concurrency. If database's low level client library provides concurrent
--  acces to one connection and/or statement then driver must support it. As
--  well as protect connection/statement from concurrent access if database does
--  not support it.
--
--  * To be clean, minimalistic, well-tested and correct base for other
--  higher-level packages.

-- * Out of scope

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

-- ** Differences in SqlValue

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
--
--  * 'SqlUUID' is added because there is many databases supporting UUID
--  natively.

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

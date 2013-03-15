{-# LANGUAGE
    TypeFamilies
  , DeriveDataTypeable
  , ExistentialQuantification
  , FlexibleContexts
  , ScopedTypeVariables
 #-}

{- |
   Module     : Database.HDBC.Types
   Copyright  : Copyright (C) 2005-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Types for HDBC.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Database.HDBC.Types
       (
         -- * Typeclasses
         Connection(..)
       , Statement(..)
         -- * Data types
       , ConnStatus(..)
       , ConnWrapper(..)
       , StatementStatus(..)
       , StmtWrapper(..)
       , SqlError(..)
         -- * functions
       , castConnection
       , withTransaction
       , withStatement
       ) where

import Prelude hiding (catch)
import qualified Data.Text.Lazy as TL
import Database.HDBC.SqlValue (SqlValue)

import Control.Exception (Exception(..), SomeException, try, catch, throwIO, bracket)
import Data.Typeable


-- | Error throwing by driver when database operation fails
data SqlError = SqlError { seNativeError :: Int -- ^ Low level database-specific error code
                         , seErrorMsg :: String -- ^ Error description from the database driver
                         }
              deriving (Eq, Show, Read, Typeable)

instance Exception SqlError


-- | Connection status
data ConnStatus = ConnOK           -- ^ Successfully connected
                | ConnDisconnected -- ^ Successfully disconnected, all
                                   -- statements must be closed at this state
                | ConnBad          -- ^ Some bad situation

-- | Typeclass to abstract the working with connection.                  
class (Typeable conn, (Statement (ConnStatement conn))) => Connection conn where
  
  -- | Statement overloded type for specific connection type
  type ConnStatement conn :: *
  
  -- | Disconnection from the database, every opened statement must be finished
  -- after this method executed. 
  disconnect :: conn -> IO ()

  -- | Explicitly start the transaction. Without starting the transaction you
  -- can not commit or rollback it. HDBC does not check if transaction started
  -- or not, this is the application's resposibility.
  --
  -- This is not recomended to use 'start' by hands, use
  -- 'Database.HDBC.Utils.withTransaction' instead
  start :: conn -> IO ()

  -- | Explicitly commit started transaction. You must 'start' the transaction
  -- before 'commit'
  --
  -- This is not recomended to use 'commit' by hands, use
  -- 'Database.HDBC.Utils.withTransaction' instead
  commit :: conn -> IO ()

  -- | Rollback the transaction's state. You must 'start' the transaction before
  -- 'rollback'
  --
  -- This is not recomended to use 'rollback' by hands, use
  -- 'Database.HDBC.Utils.withTransaction' instead
  rollback :: conn -> IO ()

  -- | Check if current connection is in transaction state. Return True if
  -- transaction is started. Each driver implements it with it's own way: some
  -- RDBMS has API to check transaction state (like PostgreSQL), some has no
  -- (like Sqlite3), so this drivers just save some flag 
  inTransaction :: conn -> IO Bool

  connStatus :: conn -> IO ConnStatus

  -- | Prepare the statement. Some databases has no feature of preparing
  -- statements (PostgreSQL can just prepare named statements), so each driver
  -- behaves it's own way.
  prepare :: conn -> TL.Text -> IO (ConnStatement conn)

  -- | Run query and safely finalize statement after that
  run :: conn -> TL.Text -> [SqlValue] -> IO ()
  run conn query values = withStatement conn query $
                          \s -> execute s values

  -- | Run raw query without parameters and safely finalize statement
  runRaw :: conn -> TL.Text -> IO ()
  runRaw conn query = withStatement conn query executeRaw
  
  -- | run executeMany and safely finalize statement 
  runMany :: conn -> TL.Text -> [[SqlValue]] -> IO ()
  runMany conn query values = withStatement conn query $
                              \s -> executeMany s values
  

  -- | Clone the database connection. Return new connection with the same
  -- settings
  clone :: conn -> IO conn

  -- | The name of the HDBC driver module for this connection. Ideally would be
  -- the same as the database name portion of the Cabal package name.  For
  -- instance, \"sqlite3\" or \"postgresql\".  This is the layer that is bound most
  -- tightly to HDBC
  hdbcDriverName :: conn -> String

  -- | The version of the C (or whatever) client library that the HDBC driver
  -- module is bound to.  The meaning of this is driver-specific.  For an ODBC
  -- or similar proxying driver, this should be the version of the ODBC library,
  -- not the eventual DB client driver.
  hdbcClientVer :: conn -> String

  -- | In the case of a system such as ODBC, the name of the database
  -- client\/server in use, if available. For others, identical to
  -- 'hdbcDriverName'.
  proxiedClientName :: conn -> String

  -- | In the case of a system such as ODBC, the version of the database client
  -- in use, if available.  For others, identical to 'hdbcClientVer'. This is
  -- the next layer out past the HDBC driver.
  proxiedClientVer :: conn -> String

  -- | The version of the database server, if available.
  dbServerVer :: conn -> String

  -- | Whether or not the current database supports transactions. If False, then
  -- 'commit' and 'rollback' should be expected to raise errors.
  dbTransactionSupport :: conn -> Bool


-- | Wrapps the specific connection. You can write database-independent code
-- mixing it with database-dependent using 'castConnection' function to cast
-- Wrapper to specific connection type, if you need.
data ConnWrapper = forall conn. Connection conn => ConnWrapper conn
                   deriving (Typeable)

instance Connection ConnWrapper where
  type ConnStatement ConnWrapper = StmtWrapper

  disconnect (ConnWrapper conn) = disconnect conn
  start (ConnWrapper conn) = start conn
  commit (ConnWrapper conn) = commit conn
  rollback (ConnWrapper conn) = rollback conn
  inTransaction (ConnWrapper conn) = inTransaction conn
  connStatus (ConnWrapper conn) = connStatus conn
  prepare (ConnWrapper conn) str = (prepare conn str) >>= (\s -> return $ StmtWrapper s)
  clone (ConnWrapper conn) = (clone conn) >>= (\c -> return $ ConnWrapper c)
  hdbcDriverName (ConnWrapper conn) = hdbcDriverName conn
  hdbcClientVer (ConnWrapper conn) = hdbcClientVer conn
  proxiedClientName (ConnWrapper conn) = proxiedClientName conn
  proxiedClientVer (ConnWrapper conn) = proxiedClientVer conn
  dbServerVer (ConnWrapper conn) = dbServerVer conn
  dbTransactionSupport (ConnWrapper conn) = dbTransactionSupport conn

-- | Cast wrapped connection to the specific connection type using 'cast' of
-- 'Typeable'. You can write database-specific code safely casting wrapped
-- connection to specific type dynamically.
castConnection :: (Connection conn) => ConnWrapper -> Maybe conn
castConnection (ConnWrapper conn) = cast conn
  


-- | Statement's status returning by function 'statementStatus'.
data StatementStatus = StatementNew      -- ^ Newly created statement
                     | StatementExecuted -- ^ Expression executed
                     | StatementFinished -- ^ Finished, fetching rows will return 'Nothing'

                       
class (Typeable stmt) => Statement stmt where

  -- | Execute single query with parameters. In query each parameter must be
  -- replaced with ''?'' placeholder. This rule is true for every database, even
  -- for PostgreSQL which uses placeholders like ''$1''. Application must ensure
  -- that the count of placeholders is equal to count of parameter, it is likely
  -- cause an error if it is not.
  execute :: stmt -> [SqlValue] -> IO ()

  -- | Execute single query without parameters. Has default implementation
  -- through 'execute'.
  executeRaw :: stmt -> IO ()
  executeRaw stmt = execute stmt []

  -- | Execute one query many times with a list of paramters. Has default
  -- implementation through 'execute'.
  executeMany :: stmt -> [[SqlValue]] -> IO ()
  executeMany stmt vals = mapM_ (execute stmt) vals

  -- | Return the current statement's status.
  statementStatus :: stmt -> IO StatementStatus

  -- | Return the count of rows affected by INSERT, UPDATE or DELETE
  -- query. After executing SELECT query it will return 0 every time.
  affectedRows :: stmt -> IO Integer

  -- | Finish statement and remove database-specific pointer. No any actions may
  -- be proceeded after closing the statement, excpet 'statementStatus' which
  -- will return 'StatementFinished'.
  finish :: stmt -> IO ()

  -- | Reset statement to it's initial state.
  reset :: stmt -> IO ()

  -- | Fetch next row from the executed statement. Return Nothing when there is
  -- no more results acceptable. Each call return next row from the result.
  -- 
  -- UPDATE INSERT and DELETE queries will likely return Nothing.
  -- 
  -- NOTE: You still need to finish explicitly the statement after receiving
  -- Nothing, unlike with old HDBC interface.
  fetchRow :: stmt -> IO (Maybe [SqlValue])

  -- | Return list of column names of the result.
  getColumnNames :: stmt -> IO [String]

  -- | Return the original executed query.
  originalQuery :: stmt -> String

-- | Wrapper around some specific 'Statement' instance to write
-- database-independent code
data StmtWrapper = forall stmt. Statement stmt => StmtWrapper stmt
                   deriving (Typeable)

instance Statement StmtWrapper where
  execute (StmtWrapper stmt) = execute stmt
  executeRaw (StmtWrapper stmt) = executeRaw stmt
  executeMany (StmtWrapper stmt) = executeMany stmt
  statementStatus (StmtWrapper stmt) = statementStatus stmt
  affectedRows (StmtWrapper stmt) = affectedRows stmt
  finish (StmtWrapper stmt) = finish stmt
  reset (StmtWrapper stmt) = reset stmt
  fetchRow (StmtWrapper stmt) = fetchRow stmt
  getColumnNames (StmtWrapper stmt) = getColumnNames stmt
  originalQuery (StmtWrapper stmt) = originalQuery stmt


{- | Execute some code.  If any uncaught exception occurs, run
'rollback' and re-raise it.  Otherwise, run 'commit' and return.

This function, therefore, encapsulates the logical property that a transaction
is all about: all or nothing.

The 'IConnection' object passed in is passed directly to the specified
function as a convenience.

This function traps /all/ uncaught exceptions, not just SqlErrors.  Therefore,
you will get a rollback for any exception that you don't handle.  That's
probably what you want anyway.

Since all operations in HDBC are done in a transaction, this function doesn't
issue an explicit \"begin\" to the server.  You should ideally have
called 'Database.HDBC.commit' or 'Database.HDBC.rollback' before
calling this function.  If you haven't, this function will commit or rollback
more than just the changes made in the included action.

If there was an error while running 'rollback', this error will not be
reported since the original exception will be propogated back.  (You'd probably
like to know about the root cause for all of this anyway.)  Feedback
on this behavior is solicited.
-}
withTransaction :: Connection conn => conn -> (conn -> IO a) -> IO a
withTransaction conn func = do
  r <- try (func conn)
  case r of
    Right x -> do
      commit conn
      return x
    Left (e :: SomeException) -> do
      catch  (rollback conn) (\(_ :: SomeException) -> return ())
      throwIO e

{-| Create statement and execute monadic action using 
it. Safely finalize Statement after action is done.
-}
withStatement :: (Connection conn, Statement stmt, stmt ~ (ConnStatement conn))
                 => conn          -- ^ Connection
                 -> TL.Text       -- ^ Query string
                 -> (stmt -> IO a) -- ^ Action around statement
                 -> IO a          -- ^ Result of action
withStatement conn query = bracket
                           (prepare conn query)
                           finish


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

Haskell database interface library.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Database.HDBC 
    (-- * Database Handles
     Connection,
     disconnect, run, prepare, commit, rollback, 
     -- * Statements
     sExecute, sExecuteMany, isActive, finish, fetchRow,
     -- * Exceptions
     SqlError(..),
     catchSql, handleSql, sqlExceptions, handleSqlError
    )

where
import Database.HDBC.Types
import Control.Exception


catchSql :: IO a -> (SqlError -> IO a) -> IO a
catchSql = catchDyn

handleSql :: (SqlError -> IO a) -> IO a -> IO a
handleSql h f = catchDyn f h

sqlExceptions :: Exception -> Maybe SqlError
sqlExceptions e = dynExceptions e >>= fromDynamic

{- | Propogate SQL exceptions to IO monad. -}
handleSqlError :: IO a -> IO a
handleSqlError action =
    catchSql action handler
    where handler e = fail ("SQL error: " ++ show e)

{-  Execute some code.  If any uncaught exception occurs, run
'rollback' and re-raise it.  Otherwise, run 'commit' and return. -}
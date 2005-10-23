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
     Statement(..)

    )

where

{- | Main database handle object.

This object must be created by a driver for a specific database (see
the database driver modules).


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
-}
                disconnect :: IO (),
                {- | Commit any pending data to the database.

                   Required to make any changes take effect. -}
                commit :: IO (),
                {- | Roll back to the state the database was in prior to the
                   last 'commit' or 'rollback'. -}
                rollback :: IO (),
                {- | Execute a single SQL statement.  Returns the number
                   of rows effected. -}
                run :: String -> IO Integer,
                {- | Prepares a statement for execution. -}
                prepare :: String -> IO Statement
               }

data Statement = Statement
    {sexecute :: [String] -> IO Integer,
     sexecuteMany :: [[String]] -> IO Integer,
     isActive :: IO Bool,
     finish :: IO (),
     
    }


                
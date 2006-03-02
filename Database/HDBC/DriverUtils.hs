{-
Copyright (C) 2006 John Goerzen <jgoerzen@complete.org>

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
   Module     : Database.HDBC.DriverUtils
   Copyright  : Copyright (C) 2006 John Goerzen
   License    : GNU LGPL, version 2.1 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Utilities for database backend drivers.

Please note: this module is intended for authors of database driver libraries
only.  Authors of applications using HDBC should use 'Database.HDBC'
exclusively.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Database.HDBC.DriverUtils (
                                  ChildList,
                                  closeAllChildren,
                                  addChild
                                 )

where
import Control.Concurrent.MVar
import System.Mem.Weak
import Database.HDBC.Types
import Control.Monad

type ChildList = MVar [Weak Statement]

{- | Close all children.  Intended to be called by the 'disconnect' function
in 'Connection'. 

There may be a potential race condition wherein a call to newSth at the same
time as a call to this function may result in the new child not being closed.
-}
closeAllChildren :: ChildList -> IO ()
closeAllChildren mcl = 
    do children <- readMVar mcl
       mapM_ closefunc children
    where closefunc child =
              do c <- deRefWeak child
                 case c of
                   Nothing -> return ()
                   Just x -> finish x

{- | Adds a new child to the existing list.  Also takes care of registering
a finalizer for it, to remove it from the list when possible. -}
addChild :: ChildList -> Statement -> IO ()
addChild mcl stmt =
    do weakptr <- mkWeakPtr stmt (Just (childFinalizer mcl))
       modifyMVar_ mcl (\l -> return (weakptr : l))

{- | The general finalizer for a child.

It is simply a filter that removes any finalized weak pointers from the parent.

If the MVar is locked at the start, does nothing to avoid deadlock.  Future
runs would probably catch it anyway. -}
childFinalizer :: ChildList -> IO ()
childFinalizer mcl = 
    do c <- tryTakeMVar mcl
       case c of
         Nothing -> return ()
         Just cl ->
             do newlist <- filterM filterfunc cl
                putMVar mcl newlist
    where filterfunc c =
              do dc <- deRefWeak c
                 case dc of
                   Nothing -> return False
                   Just _ -> return True

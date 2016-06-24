{-# LANGUAGE RankNTypes, ConstraintKinds, KindSignatures, ConstraintKinds, ScopedTypeVariables #-}

module Control.Monad.HDBC
    (
      disconnectW
    , commitW
    , rollbackW
    , runRawW
    , runW
    , prepareW
    , cloneW
    , getTablesW
    , describeTableW
    , WithCon
    , runWith
    , module Control.Monad.Reader
    , module Database.HDBC
    )
where

import Control.Monad.Reader
import Database.HDBC

type WithCon con m a = (IConnection con, MonadIO m) => ReaderT con m a

runWith :: (IConnection con, MonadIO m) => WithCon con m a -> con -> m a
runWith = runReaderT

lift1WC :: (IConnection con) => (con -> IO a) -> WithCon con m a

lift1WC action = do
  con <- ask
  a <- liftIO $ action con
  return a

lift2WC :: (IConnection con) => (con -> x -> IO a) -> x -> WithCon con m a

lift2WC action = lift1WC . flip action

disconnectW :: WithCon con m ()
disconnectW = lift1WC disconnect

commitW :: WithCon con m ()
commitW = lift1WC commit

rollbackW :: WithCon con m ()
rollbackW = lift1WC rollback

runRawW :: String
       -> WithCon con m ()
runRawW = lift2WC runRaw

runW :: String
    -> [SqlValue]
    -> WithCon con m Integer
runW sql = lift1WC . flip (flip run sql)

prepareW :: String
        -> WithCon con m Statement
prepareW = lift2WC prepare

cloneW :: WithCon con m con
cloneW = lift1WC clone

getTablesW :: WithCon con m [String]
getTablesW = lift1WC getTables

describeTableW :: String
              -> WithCon con m [(String, SqlColDesc)]
describeTableW = lift2WC describeTable

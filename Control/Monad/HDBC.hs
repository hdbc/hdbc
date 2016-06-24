{-# LANGUAGE RankNTypes, ConstraintKinds, KindSignatures, ConstraintKinds, ScopedTypeVariables #-}

module Control.Monad.HDBC.Class
    (
      disconnectW, commitW, rollbackW, runRawW
    , runW, prepareW, cloneW
    , getTablesW, describeTableW
    , module HDBC
    )
where

import Control.Monad.Reader
import Data.Convertible
import Database.HDBC as HDBC

type WithCon con m a = (IConnection con, MonadIO m) => ReaderT con m a

lift1WC :: (IConnection con) => (con -> IO a) -> WithCon con m a

lift1WC action = do
  con <- ask
  a <- liftIO $ action con
  return a

lift2WC :: (IConnection con) => (con -> x -> IO a) -> x -> WithCon con m a

lift2WC action = lift1WC . flip action

disconnectW :: WithCon con m ()
disconnectW = lift1WC HDBC.disconnect

commitW :: WithCon con m ()
commitW = lift1WC HDBC.commit

rollbackW :: WithCon con m ()
rollbackW = lift1WC HDBC.rollback

runRawW :: String
       -> WithCon con m ()
runRawW = lift2WC HDBC.runRaw

runW :: String
    -> [SqlValue]
    -> WithCon con m Integer
runW sql = lift1WC . flip (flip HDBC.run sql)

prepareW :: String
        -> WithCon con m Statement
prepareW = lift2WC HDBC.prepare

cloneW :: WithCon con m con
cloneW = lift1WC HDBC.clone

getTablesW :: WithCon con m [String]
getTablesW = lift1WC HDBC.getTables

describeTableW :: String
              -> WithCon con m [(String, SqlColDesc)]
describeTableW = lift2WC HDBC.describeTable

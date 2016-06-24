{-# LANGUAGE RankNTypes, ConstraintKinds, KindSignatures, ConstraintKinds, ScopedTypeVariables #-}

module Control.Monad.HDBC
    (
      IConnection
    , SqlValue
    , Statement
    , SqlColDesc
    , disconnect, commit, rollback, runRaw
    , run, prepare, clone, hdbcDriverName 
    , hdbcClientVer, dbServerVer, dbTransactionSupport
    , getTables, describeTable, proxiedClientName
    )
where

import Control.Monad.Reader
import qualified Database.HDBC as HDBC

type IConnection = HDBC.IConnection
type SqlValue = HDBC.SqlValue
type Statement = HDBC.Statement
type SqlColDesc = HDBC.SqlColDesc

type WithCon con m a = (IConnection con, MonadIO m) => ReaderT con m a

lift1WC :: (IConnection con) => (con -> IO a) -> WithCon con m a

lift1WC action = do
  con <- ask
  a <- liftIO $ action con
  return a

lift2WC :: (IConnection con) => (con -> x -> IO a) -> x -> WithCon con m a

lift2WC action = lift1WC . flip action

disconnect :: WithCon con m ()
disconnect = lift1WC HDBC.disconnect

commit :: WithCon con m ()
commit = lift1WC HDBC.commit

rollback :: WithCon con m ()
rollback = lift1WC HDBC.rollback

runRaw :: String
       -> WithCon con m ()
runRaw = lift2WC HDBC.runRaw

run :: String
    -> [SqlValue]
    -> WithCon con m Integer
run sql = lift1WC . flip (flip HDBC.run sql)

prepare :: String
        -> WithCon con m Statement
prepare = lift2WC HDBC.prepare

clone :: WithCon con m con
clone = lift1WC HDBC.clone

hdbcDriverName :: IConnection con 
               => con
               -> String
hdbcDriverName = HDBC.hdbcDriverName

hdbcClientVer :: IConnection con 
              => con 
              -> String
hdbcClientVer = HDBC.hdbcClientVer

proxiedClientName :: IConnection con 
                  => con 
                  -> String
proxiedClientName = HDBC.proxiedClientName

dbServerVer :: IConnection con 
            => con 
            -> String
dbServerVer = HDBC.dbServerVer

dbTransactionSupport :: IConnection con 
                     => con 
                     -> Bool
dbTransactionSupport = HDBC.dbTransactionSupport

getTables :: WithCon con m [String]
getTables = lift1WC HDBC.getTables

describeTable :: String
              -> WithCon con m [(String, SqlColDesc)]
describeTable = lift2WC HDBC.describeTable

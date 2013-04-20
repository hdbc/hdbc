{-# LANGUAGE
  DeriveDataTypeable
, TypeFamilies
, OverloadedStrings
, ScopedTypeVariables
  #-}

module Main where

import System.Mem
import System.Mem.Weak

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Hspec.Expectations

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Functor
import Data.Typeable
import Data.Maybe

import qualified Data.Text.Lazy as TL

import Database.HDBC
import Database.HDBC.DriverUtils

data TStatus = TIdle | TInTransaction
             deriving (Eq, Show, Read, Typeable)

data DummyConnection =
  DummyConnection { dcState :: MVar ConnStatus
                  , dcTrans :: MVar TStatus
                  , dcChilds :: ChildList DummyStatement
                  , dcTransSupport :: Bool
                  }
  deriving (Typeable, Eq)

data DummyStatement =
  DummyStatement { dsConnection :: DummyConnection
                 , dsQuery :: TL.Text
                 , dsStatus :: MVar StatementStatus
                 }
  deriving (Typeable, Eq)
  

newConnection transSupport = DummyConnection
                             <$> newMVar ConnOK
                             <*> newMVar TIdle
                             <*> newChildList
                             <*> return transSupport

withOKConnection :: DummyConnection -> IO a -> IO a
withOKConnection conn action = do
  st <- connStatus conn
  case st of
    ConnOK -> action
    _ -> throwIO $ SqlError "1" $ "Connection has wrong status " ++ show st

withTransactionSupport :: DummyConnection -> IO a -> IO a
withTransactionSupport conn action = case (dcTransSupport conn) of
  True -> action
  False -> throwIO $ SqlError "10" "Transaction is not supported by this connection"
  

instance Connection DummyConnection where
  type ConnStatement DummyConnection = DummyStatement
  disconnect conn = modifyMVar_ (dcState conn) $ \_ -> do
    closeAllChildren $ dcChilds conn
    return ConnDisconnected
  
  begin conn = withOKConnection conn
               $ withTransactionSupport conn
               $ modifyMVar_ (dcTrans conn)
               $ \s -> case s of
    TIdle -> return TInTransaction
    TInTransaction -> throwIO $ SqlError "2" $ "Connection is already in transaction "
  commit conn = withOKConnection conn
                $ withTransactionSupport conn
                $ modifyMVar_ (dcTrans conn)
                $ \s -> case s of
    TInTransaction -> return TIdle
    TIdle -> throwIO $ SqlError "3" $ "Connection is not in transaction to commit"
  rollback conn = withOKConnection conn
                  $ withTransactionSupport conn
                  $ modifyMVar_ (dcTrans conn)
                  $ \s -> case s of
    TInTransaction -> return TIdle
    TIdle -> throwIO $ SqlError "4" $ "Connection is not in transaction to rollback"
  inTransaction conn = withTransactionSupport conn $ do
    t <- readMVar $ dcTrans conn
    return $ t == TInTransaction
  connStatus = readMVar . dcState
  prepare conn query = do
    st <- DummyStatement
          <$> return conn
          <*> return query
          <*> newMVar StatementNew
    addChild (dcChilds conn) st
    return st
  clone conn = DummyConnection
               <$> (newMVar ConnOK)
               <*> (newMVar TIdle)
               <*> newChildList
               <*> (return $ dcTransSupport conn)
  hdbcDriverName = const "DummyDriver"
  dbTransactionSupport = dcTransSupport

  
instance Statement DummyStatement where
  execute stmt _ = modifyMVar_ (dsStatus stmt) $ \st -> do
    case st of
      StatementNew -> do
        if (originalQuery stmt) == "throw"
          then throwIO $ SqlError "5" "Throwed query exception"
          else return StatementExecuted
      _ -> throwIO $ SqlError "6" $ "Statement has wrong status to execute query " ++ show st
    
  statementStatus = readMVar . dsStatus

  affectedRows = const $ return 0
  finish stmt = modifyMVar_ (dsStatus stmt) $ const $ return StatementFinished
  reset stmt = modifyMVar_ (dsStatus stmt) $ const $ return StatementNew
  fetchRow = const $ return Nothing
  getColumnNames = const $ return []
  originalQuery = dsQuery



test1 = do
  c <- newConnection True
  (withTransaction c $ do
      intr <- inTransaction c
      intr `shouldBe` True
      stmt <- prepare c "throw"  -- cause an exception throwing
      executeRaw stmt
    ) `shouldThrow` (\(_ :: SqlError) -> True)
  intr <- inTransaction c
  intr `shouldBe` False         -- after rollback

test2 = do
  c <- newConnection True
  intr1 <- inTransaction c
  intr1 `shouldBe` False
  withTransaction c $ do
    stmt <- prepare c "dummy query"
    executeRaw stmt
    intr <- inTransaction c
    intr `shouldBe` True
  intr <- inTransaction c
  intr `shouldBe` False         -- after commit

test3 = do
  c <- newConnection False
  sub c
  performGC                     -- after this all refs must be empty
  p <- readMVar $ dcChilds c
  prts <- filterM (deRefWeak >=> (return . isJust)) p
  (length prts) `shouldBe` 0

    where
      sub c = do
        st1 <- prepare c "query 1"
        st2 <- prepare c "query 2"
        executeRaw st2
        prts <- readMVar $ dcChilds c
        (length prts) `shouldBe` 2

test4 = do
  c <- newConnection False
  stmt <- prepare c "query 1"
  disconnect c
  ss <- statementStatus stmt
  ss `shouldBe` StatementFinished

test5 = do
  c <- newConnection True
  c2 <- clone c
  st1 <- prepare c "query"
  stt <- statementStatus st1
  stt `shouldBe` StatementNew
  st2 <- prepare c2 "query"
  stt2 <- statementStatus st2
  stt2 `shouldBe` StatementNew
  disconnect c
  stt <- statementStatus st1
  stt `shouldBe` StatementFinished
  stt2 <- statementStatus st2
  stt2 `shouldBe` StatementNew
  disconnect c2
  stt2 <- statementStatus st2
  stt2 `shouldBe` StatementFinished
  
  
main :: IO ()
main = defaultMain [ testCase "Transaction exception handling" test1
                   , testCase "Transaction commiting" test2
                   , testCase "Child statements" test3
                   , testCase "Childs closed when disconnect" test4
                   , testCase "Each connection has it's own childs" test5
                   ]

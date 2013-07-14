{-# LANGUAGE
  DeriveDataTypeable
, TypeFamilies
, OverloadedStrings
, ScopedTypeVariables
  #-}

module DummyDriver where

import System.Mem
import System.Mem.Weak

import Test.HUnit (Assertion)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Hspec.Expectations

import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Typeable
import Data.Maybe

import Database.HDBC
import Database.HDBC.DriverUtils

data TStatus = TIdle | TInTransaction
             deriving (Eq, Show, Read, Typeable)

data DummyConnection =
  DummyConnection { dcState :: MVar ConnStatus
                  , dcTrans :: MVar TStatus
                  , dcData :: MVar [[SqlValue]]
                  , dcChilds :: ChildList DummyStatement
                  , dcTransSupport :: Bool
                  }
  deriving (Typeable, Eq)

data DummyStatement =
  DummyStatement { dsConnection :: DummyConnection
                 , dsQuery :: Query
                 , dsSelecting :: MVar (Maybe Int)
                 , dsStatus :: MVar StatementStatus
                 }
  deriving (Typeable, Eq)
  

newConnection :: Bool -> IO DummyConnection
newConnection transSupport = DummyConnection
                             <$> newMVar ConnOK
                             <*> newMVar TIdle
                             <*> newMVar []
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
          <*> newMVar Nothing
          <*> newMVar StatementNew
    addChild (dcChilds conn) st
    return st
  clone conn = DummyConnection
               <$> (newMVar ConnOK)
               <*> (newMVar TIdle)
               <*> newMVar []
               <*> newChildList
               <*> (return $ dcTransSupport conn)
  hdbcDriverName = const "DummyDriver"
  dbTransactionSupport = dcTransSupport

  
instance Statement DummyStatement where
  execute stmt params = modifyMVar_ (dsStatus stmt) $ \st -> do
    case st of
      StatementNew -> do
        case originalQuery stmt of
          "throw" -> throwIO $ SqlError "5" "Throwed query exception"
          "insert" -> modifyMVar (dcData $ dsConnection stmt) $ \d -> return (d ++ [params], StatementExecuted)
          "select" -> modifyMVar (dsSelecting stmt) $ const $ return (Just 0, StatementExecuted)
          _ -> return StatementExecuted
      _ -> throwIO $ SqlError "6" $ "Statement has wrong status to execute query " ++ show st
    
  statementStatus = readMVar . dsStatus

  affectedRows = const $ return 0
  finish stmt = modifyMVar_ (dsStatus stmt) $ const $ return StatementFinished
  reset stmt = modifyMVar_ (dsStatus stmt) $ const $ return StatementNew
  fetchRow stmt = modifyMVar (dsSelecting stmt) $ \slct -> case slct of
    Nothing -> return (Nothing, Nothing)
    Just sl -> do
      dt <- readMVar $ dcData $ dsConnection stmt
      if (length dt) > sl
        then return (Just $ sl+1, Just $ dt !! sl)
        else return (Nothing, Nothing)
    
    
  getColumnNames = const $ return []
  originalQuery = dsQuery


test1 :: Assertion
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

test2 :: Assertion
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

test3 :: Assertion
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

test4 :: Assertion
test4 = do
  c <- newConnection False
  stmt <- prepare c "query 1"
  disconnect c
  ss <- statementStatus stmt
  ss `shouldBe` StatementFinished

test5 :: Assertion
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

testFetchAllRows :: Assertion
testFetchAllRows = do
  c <- newConnection True
  let indt = [ [SqlInt32 10, SqlText "hello"]
             , [SqlDouble 45.4, SqlNull]
             , [SqlText "sdf", SqlText "efef"]
             ]
  s <- prepare c "insert"
  execute s $ indt !! 0
  finish s
  s2 <- prepare c "insert"
  execute s2 $ indt !! 1
  finish s2
  s3 <- prepare c "insert"
  execute s3 $ indt !! 2
  finish s3
  ss <- prepare c "select"
  executeRaw ss
  outdt <- fetchAllRows ss
  outdt `shouldBe` indt
  
  
  
main :: IO ()
main = defaultMain [ testCase "Transaction exception handling" test1
                   , testCase "Transaction commiting" test2
                   , testCase "Child statements" test3
                   , testCase "Childs closed when disconnect" test4
                   , testCase "Each connection has it's own childs" test5
                   , testCase "Fetch all rows preserve order" testFetchAllRows
                   ]

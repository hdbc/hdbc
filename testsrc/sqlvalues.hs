{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts
           , CPP #-}

module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Gen(..), Arbitrary(..))
import Test.QuickCheck.Property
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Assertions
import Database.HDBC.SqlValue (toSql, fromSql, SqlValue)

import Control.Applicative

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Word
import Data.Decimal
import Data.Time
import Data.Fixed
import Data.Convertible (Convertible(..))
import qualified System.Time as ST

import Debug.Trace(trace)

ts s = trace (show s) s

#if MIN_VERSION_Decimal(0,2,4)
-- Decimal-0.2.4 has no Arbitrary instance in library any more
instance (Arbitrary i, Integral i) => Arbitrary (DecimalRaw i) where
  arbitrary = Decimal <$> arbitrary <*> arbitrary
#endif


commonChecks :: (Convertible a SqlValue, Convertible SqlValue a, Eq a, Show a) => a -> Property
commonChecks x = (x ==? (fromSql $ toSql x)) .&&. 
                 (x ==? (fromSql $ toSql (fromSql $ toSql x :: B.ByteString)))

  
sqlvalues :: Spec
sqlvalues = describe "SqlValue should be convertible" $ do
  prop "with string" $ \(s::String) -> commonChecks s
  prop "with text" $ \(t::T.Text) -> commonChecks t
  prop "with lazy text" $ \(t::TL.Text) -> commonChecks t
  prop "with bytestring" $ \(b::B.ByteString) -> commonChecks b
  prop "with lazy bytestring" $ \(b::BL.ByteString) -> commonChecks b
  prop "with int" $ \(i :: Int) -> commonChecks i
  prop "with int32" $ \(i :: Int32) -> commonChecks i
  prop "with int64" $ \(i :: Int64) -> commonChecks i 
  prop "with word32" $ \(w :: Word32) -> commonChecks w 
  prop "with word64" $ \(w :: Word64) -> commonChecks w 
  prop "with Integer" $ \(i :: Integer) -> commonChecks i 
  prop "with Bool" $ \(b :: Bool) -> commonChecks b 
  prop "with Double" $ \(d :: Double) -> commonChecks d
  prop "with Decimal" $ \(d :: Decimal) -> commonChecks d
  prop "with Day" $ \(d :: Day) -> commonChecks d
  prop "with TimeOfDay" $ \(tod :: TimeOfDay) -> commonChecks tod
  prop "with LocalTime" $ \(lt :: LocalTime) -> commonChecks lt
  prop "with UTCTime" $ \(ut :: UTCTime) -> commonChecks ut
  prop "with Maybe Int" $ \(mi :: Maybe Int) -> mi == (fromSql $ toSql mi) -- can not represent Null as ByteString

main = do
  hspec $ sqlvalues

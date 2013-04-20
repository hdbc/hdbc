{-# LANGUAGE ScopedTypeVariables
           , FlexibleContexts
           , CPP #-}

module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck (Gen(..), Arbitrary(..))
import Test.QuickCheck.Property
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Assertions
import Database.HDBC (SqlValue, toSql, fromSql)

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

import Debug.Trace(trace)

ts s = trace (show s) s

#if MIN_VERSION_Decimal(0,3,1)
-- Decimal-0.2.4 has no Arbitrary instance in library any more
instance (Arbitrary i, Integral i) => Arbitrary (DecimalRaw i) where
  arbitrary = Decimal <$> arbitrary <*> arbitrary
#endif


commonChecks :: (Convertible a SqlValue, Convertible SqlValue a, Eq a, Show a) => a -> Property
commonChecks x = (partialChecks x) .&&. 
                 (x ==? (fromSql $ toSql (fromSql $ toSql x :: TL.Text)))

partialChecks x = x ==? (fromSql $ toSql x)

  
main = defaultMain [ testProperty "with string" $ \(s::String) -> commonChecks s
                   , testProperty "with text" $ \(t::T.Text) -> commonChecks t
                   , testProperty "with lazy text" $ \(t::TL.Text) -> commonChecks t
                   , testProperty "with bytestring" $ \(b::B.ByteString) -> partialChecks b
                   , testProperty "with lazy bytestring" $ \(b::BL.ByteString) -> partialChecks b
                   , testProperty "with int" $ \(i :: Int) -> commonChecks i
                   , testProperty "with int32" $ \(i :: Int32) -> commonChecks i
                   , testProperty "with int64" $ \(i :: Int64) -> commonChecks i 
                   , testProperty "with word32" $ \(w :: Word32) -> commonChecks w 
                   , testProperty "with word64" $ \(w :: Word64) -> commonChecks w 
                   , testProperty "with Integer" $ \(i :: Integer) -> commonChecks i 
                   , testProperty "with Bool" $ \(b :: Bool) -> commonChecks b 
                   , testProperty "with Double" $ \(d :: Double) -> commonChecks d
                   , testProperty "with Decimal" $ \(d :: Decimal) -> commonChecks d
                   , testProperty "with Day" $ \(d :: Day) -> commonChecks d
                   , testProperty "with TimeOfDay" $ \(tod :: TimeOfDay) -> commonChecks tod
                   , testProperty "with LocalTime" $ \(lt :: LocalTime) -> commonChecks lt
                   , testProperty "with UTCTime" $ \(ut :: UTCTime) -> commonChecks ut
                   , testProperty "with Maybe Int" $ \(mi :: Maybe Int) -> mi == (fromSql $ toSql mi) -- can not represent Null as ByteString
                   ]


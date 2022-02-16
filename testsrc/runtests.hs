module Main where
import qualified Test.HUnit as HU
import Test.QuickCheck (quickCheck)

import qualified TestSqlValue

main = do HU.runTestTT (HU.TestList $ TestSqlValue.allHUnitTests)
          traverse quickCheck TestSqlValue.allQuickCheckTests
          return ()

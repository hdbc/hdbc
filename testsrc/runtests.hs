module Main where
import qualified Test.HUnit as HU
import Test.HUnit.Tools

import qualified TestSqlValue

test1 = HU.TestCase ((HU.@=?) "x" "x")

alltests = [HU.TestLabel "test1" test1,
            tl "TestSqlValue" TestSqlValue.allt
           ]

main = do runVerboseTests (HU.TestList alltests)
          return ()
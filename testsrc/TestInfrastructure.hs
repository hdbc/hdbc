{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}
module TestInfrastructure where
import qualified Test.QuickCheck as QC
import qualified Test.HUnit as HU
import Test.HUnit.Tools

q :: QC.Testable a => String -> a -> HU.Test
q = qc2hu 250

qverbose :: QC.Testable a => String -> a -> HU.Test
qverbose = qc2hu 250

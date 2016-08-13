module TestHelper where

import Test.HUnit

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

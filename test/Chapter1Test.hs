module Chapter1Test where

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)
import Chapter1
import TestHelper (testCase)

itRuns::Test
itRuns = testCase "I can run tests" $ True @=? doesItRun 1

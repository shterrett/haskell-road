import Chapter1Test
import Chapter2Test
import Chapter3Test

import Test.HUnit
import System.Exit (ExitCode(..), exitWith)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

allTests::[Test]
allTests = Chapter1Test.tests ++
           Chapter2Test.tests ++
           Chapter3Test.tests

main :: IO ()
main = exitProperly (runTestTT (TestList allTests))

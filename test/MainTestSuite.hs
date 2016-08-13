import Chapter1Test

import Test.HUnit
import System.Exit (ExitCode(..), exitWith)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

allTests::[Test]
allTests = [Chapter1Test.itRuns]

main :: IO ()
main = exitProperly (runTestTT (TestList allTests))

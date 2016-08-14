module Chapter1Test where

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import Chapter1
import TestHelper (testCase)

itRuns::Test
itRuns = testCase "I can run tests" $ True @=? doesItRun 1

maxIntTests :: [Test]
maxIntTests = [ testCase "Returns max int in list" $
                5 @=? maxInt [1, 5, 3, 2, 4]
              , testCase "Returns only element in single-element list" $
                5 @=? maxInt [5]
              ]

removeFirstTests :: [Test]
removeFirstTests = [ testCase "Removes first occurence" $
                     [2, 4, 5, 4, 3, 2] @=? rmvFst 3 [2, 3, 4, 5,4, 3, 2]
                   , testCase "Removes only occurence" $
                     [6, 9] @=? rmvFst 3 [6, 3, 9]
                   , testCase "Returns empty list" $
                     [] @=? rmvFst 3 [3]
                   , testCase "Returns empty list if empty" $
                     [] @=? rmvFst 3 []
                   , testCase "Returns unchanged list if not found" $
                     [2, 3, 4] @=? rmvFst 5 [2, 3, 4]
                   ]

substringTests :: [Test]
substringTests = [ testCase "false for empty search string" $
                   False @=? substring "substr" ""
                 , testCase "true for empty sub-string" $
                   True @=? substring "" "search string"
                 , testCase "true for prefix" $
                   True @=? substring "prefix" "prefixesareeasy"
                 , testCase "true for substring" $
                   True @=? substring "substr" "there is substr in the middle"
                 , testCase "false for not present" $
                   False @=? substring "substr" "nothing here"
                 , testCase "false for almost present" $
                   False @=? substring "substr" "almost subst"
                 , testCase "True for exact match" $
                   True @=? substring "substr" "substr"
                 ]

sumLengthsTests :: [Test]
sumLengthsTests = [ testCase "sums lengths of inner lists" $
                    3 @=? sumLengths [[1, 2], [3]]
                  , testCase "0 for empty list" $
                    0 @=? sumLengths []
                  , testCase "0 for list of empty lists" $
                    0 @=? sumLengths [[], [], [], []]
                  ]

tests :: [Test]
tests = [itRuns] ++
        maxIntTests ++
        removeFirstTests ++
        substringTests ++
        sumLengthsTests

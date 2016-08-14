module Chapter2Test where

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import Chapter2
import TestHelper (testCase)

contradiction1Tests :: [Test]
contradiction1Tests = [ testCase "Returns true for a contradiction" $
                        True @=? isContradiction1 (\b -> b && not b)
                      , testCase "Returns false for a true statement" $
                        False @=? isContradiction1 (\b -> b == b)
                      ]

contradiction2Tests :: [Test]
contradiction2Tests = [ testCase "Returns true for a contradiction" $
                        True @=? isContradiction2 (\a b -> (a && not a) || (b && not b))
                      , testCase "Returns false for a true statement" $
                        False @=? isContradiction2 (\a b -> (a == a) && (b == b))
                      ]

contradiction3Tests :: [Test]
contradiction3Tests = [ testCase "Returns true for a contradiction" $
                        True @=? isContradiction3 (\a b c -> (a && not a) ||
                                                             (b && not b) ||
                                                             (c && not c))
                      , testCase "Returns false for a true statement" $
                        False @=? isContradiction3 (\a b c -> (a == a) &&
                                                              (b == b) &&
                                                              (c == c))
                      ]

tests :: [Test]
tests = contradiction1Tests ++
        contradiction2Tests ++
        contradiction3Tests

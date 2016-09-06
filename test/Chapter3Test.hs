module Chapter3Test where

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import Chapter3
import TestHelper (testCase)

productPlusOneTests :: [Test]
productPlusOneTests = [ testCase "returns the product of all primes less than prime + 1" $
                        7 @=? productPlusOne 5
                      , testCase "returns product of all primes less than composite + 1" $
                        31 @=? productPlusOne 6
                      ]

refuteTests :: [Test]
refuteTests = [ testCase "returns False" $
                False @=? refute [1..]
              ]

tests :: [Test]
tests = productPlusOneTests ++
        refuteTests

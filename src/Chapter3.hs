{-# LANGUAGE FlexibleInstances #-}

module Chapter3 where

import Book.TUOLP

-- ex 3.39
productPlusOne :: Integer -> Integer
productPlusOne bound = (foldl (*) 1 (primeList bound)) + 1
  where primeList b = takeWhile (\p -> p < b) primes

refute :: [Integer] -> Bool
refute = (all prime) . (map productPlusOne)

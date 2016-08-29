{-# LANGUAGE FlexibleInstances #-}
module Chapter2 where

import Book.TAMO

-- ex 2.15
isContradiction1 :: (Bool -> Bool) -> Bool
isContradiction1 p = not ((p False) || (p True))

isContradiction2 :: (Bool -> Bool -> Bool) -> Bool
isContradiction2 p = not (or [ p a b | a <- [True, False],
                                       b <- [True, False]])

isContradiction3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
isContradiction3 p = not (or [ p a b c | a <- [True, False],
                                         b <- [True, False],
                                         c <- [True, False]])

-- ex 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique test as = (filteredLength test as) == 1
  where filteredLength test as = length $ filter test as

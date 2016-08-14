module Chapter1 where

import Book.GS

doesItRun :: Int -> Bool
doesItRun _ = True

-- ex 1.9
maxInt :: [Int] -> Int
maxInt [] = error "Empty List"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

-- ex 1.10
rmvFst :: Int -> [Int] -> [Int]
rmvFst i [] = []
rmvFst i (x:xs) | x == i = xs
                | otherwise = [x] ++ rmvFst i xs

-- ex 1.17
substring :: String -> String -> Bool
substring "" "" = True
substring "" s = True
substring s "" = False
substring ss s@(y:ys) = Book.GS.prefix ss s || substring ss ys

-- ex 1.21
sumLengths :: [[a]] -> Int
sumLengths ll = foldl (+) 0 (map length ll)

module Book.COR

where

import System.Random (mkStdGen,randomRs)
import Book.Polynomials
import Book.PowerSeries

default (Integer, Rational, Double)

ones = 1 : ones

nats = 0 : map (+1) nats

odds = 1 : map (+2) odds

theOnes = iterate id 1
theNats = iterate (+1) 0
theOdds = iterate (+2) 1

theNats1 = 0 : zipWith (+) ones theNats1

theFibs = 0 : 1 : zipWith (+) theFibs (tail theFibs)

pr (x1:x2:x3:xs) = x1*x3 - x2*x2 : pr (x2:x3:xs)

sieve :: [Integer] -> [Integer]
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
  where
  mark (y:ys) k m | k == m    =  0 : (mark ys  1    m)
                  | otherwise =  y : (mark ys (k+1) m)

sieve' :: [Integer] -> [Integer]
sieve' (n:xs) = n : sieve' (filter (\ m -> (rem m n) /= 0) xs)

primes' :: [Integer]
primes' = sieve' [2..]

randomInts :: Int -> Int -> [Int]
randomInts bound seed = 
  tail (randomRs (0,bound) (mkStdGen seed))

type Process = [Int] -> [String]

start :: Process -> Int -> Int -> [String]
start process bound seed = process (randomInts bound seed)

clock :: Process
clock (0:xs) = "tick"  : clock xs
clock (1:xs) = "crack" : []

vending, vending1, vending2, vending3 :: Process
vending  (0:xs) = "coin"     : vending1 xs
vending  (1:xs) =              vending  xs
vending1 (0:xs) = "coin"     : vending2 xs
vending1 (1:xs) = "water"    : vending  xs
vending2 (0:xs) = "coin"     : vending3 xs
vending2 (1:xs) = "beer"     : vending  xs
vending3 (0:xs) = "moneyback": vending  xs
vending3 (1:xs) =              vending3 xs

ptd :: Process
ptd = ptd0 0

ptd0 :: Int -> Process
ptd0 0 (0:xs) = ptd0 0 xs
ptd0 i (0:xs) = ("return " ++ show i ++ " euro") : ptd0 0 xs
ptd0 i (1:xs) = "1 euro" : ptd0 (i+1) xs
ptd0 i (2:xs) = "2 euro" : ptd0 (i+2) xs
ptd0 0 (3:xs) = ptd0 0 xs
ptd0 i (3:xs) = ("ticket " ++ show (i * 20) ++ " min") : ptd0 0 xs

actions   = user [0,0,1] responses
responses = vending actions

user acts ~(r:s:p:resps) = acts ++ user (proc [r,s,p]) resps
proc ["coin","coin","beer"] =  [0,0,1]

approx :: Integer -> [a] -> [a]
approx n []  | n <= 0     = undefined
             | otherwise  = []
approx n (x:xs) = x : approx (n-1) xs

o2e :: Num a => [a] -> [a]
o2e []     = []
o2e (f:fs) = f : o2e (deriv (f:fs))

e2o ::  (Ord a, Fractional a) => [a] -> [a]
e2o []     = []
e2o (f:fs) = [f] + (int (e2o (fs)))


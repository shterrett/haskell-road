module Book.WWN

where

import Data.List
import Data.Ratio
import Data.Char
import Data.Complex
import Book.Nats

binary :: Integral a => a -> [Int]
binary x = reverse (bits x)
  where bits 0 = [0]
        bits 1 = [1]
        bits n = fromIntegral (rem n 2) : bits (quot n 2)

showDigits :: [Int] -> String
showDigits = map intToDigit

bin ::  Integral a => a -> String
bin = showDigits . binary

coprime :: Integer -> Integer -> Bool
coprime m n = (gcd m n) == 1

gcde :: Integral a => a -> a -> (a,a,a)
gcde a b | a == 0 && b == 0 = error "gcd undefined"
         | a < 0  || b < 0  = error "gcd undefined"
         | a == 0           = (b,0,1)
         |           b == 0 = (a,1,0)
         | a > b            = let
                               (k,r)   = quotRem a b
                               (d,x,y) = gcde b r
                              in (d, y, x-k*y)
         | otherwise        = let
                               (k,r)   = quotRem b a
                               (d,x,y) = gcde a r
                              in (d, x-k*y, y)

data Sgn = P | N deriving (Eq,Show)
type MyInt = (Sgn,Natural)

myplus :: MyInt -> MyInt -> MyInt
myplus (s1,m) (s2,n) | s1 == s2           = (s1,m+n)
                     | s1 == P  && n <= m = (P,m-n)
                     | s1 == P  && n > m  = (N,n-m)
                     | otherwise          = myplus (s2,n) (s1,m)

type NatPair = (Natural,Natural)

plus1 :: NatPair -> NatPair -> NatPair
plus1 (m1, m2) (n1, n2) = (m1+n1, m2+n2)

subtr1 :: NatPair -> NatPair -> NatPair
subtr1 (m1, m2) (n1, n2) = plus1 (m1, m2) (n2, n1)

mult1 :: NatPair -> NatPair -> NatPair
mult1 (m1, m2) (n1, n2) = (m1*n1 + m2*n2, m1*n2 + m2*n1)

eq1 :: NatPair -> NatPair -> Bool
eq1 (m1, m2) (n1, n2) = (m1+n2) == (m2+n1)

reduce1 :: NatPair -> NatPair
reduce1 (m1,Z) = (m1,Z)
reduce1 (Z,m2) = (Z,m2)
reduce1 (S m1, S m2) = reduce1 (m1, m2)

decExpand :: Rational -> [Integer]
decExpand x | x < 0     = error "negative argument"
            | r == 0    = [q]
            | otherwise = q : decExpand ((r*10) % d)
   where
   (q,r) = quotRem n d
   n     = numerator x
   d     = denominator x

decForm :: Rational -> (Integer,[Int],[Int])
decForm x | x < 0     = error "negative argument"
          | otherwise = (q,ys,zs)
  where
  (q,r)   = quotRem n d
  n       = numerator x
  d       = denominator x
  (ys,zs) = decF (r*10) d []

decF :: Integer -> Integer -> [(Int,Integer)] -> ([Int],[Int])
decF n d xs | r == 0        = (reverse (q: (map fst xs)),[])
            | elem (q,r) xs = (ys,zs)
            | otherwise     =  decF (r*10) d ((q,r):xs)
     where
     (q',r)  = quotRem n d
     q       = fromIntegral q'
     xs'     = reverse xs
     Just k  = elemIndex (q,r) xs'
     (ys,zs) = splitAt k (map fst xs')

mechanicsRule :: Rational -> Rational -> Rational
mechanicsRule p x = (1 % 2) * (x + (p * (recip x)))

mechanics :: Rational -> Rational -> [Rational]
mechanics p x = iterate (mechanicsRule p) x

sqrtM :: Rational -> [Rational]
sqrtM p | p < 0     = error "negative argument"
        | otherwise = mechanics p s
   where
   s = if xs == [] then 1 else last xs
   xs = takeWhile  (\ m -> m^2 <= p) [1..]

approximate :: Rational -> [Rational] -> Rational
approximate eps (x:y:zs)
  | abs (y-x) < eps = y
  | otherwise       = approximate eps (y:zs)

apprx :: [Rational] -> Rational
apprx = approximate (1/10^6)

mySqrt :: Rational -> Rational
mySqrt p = apprx (sqrtM p)

solveQ :: (Complex Float, Complex Float, Complex Float)
                               -> (Complex Float, Complex Float)
solveQ =  \ (a,b,c) -> if a == 0 then error "not quadratic"
                                 else let d = b^2 - 4*a*c in
                                           ((- b + sqrt d) / 2*a,
                                            (- b - sqrt d) / 2*a)

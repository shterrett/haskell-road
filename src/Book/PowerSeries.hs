module Book.PowerSeries

where

import Book.Polynomials

default (Integer, Rational, Double)

instance (Ord a, Fractional a) => Fractional [a] where
  fromRational c  = [fromRational c]
  fs     / []     = error "division by 0 attempted"
  []     / gs     = []
  (0:fs) / (0:gs) = fs / gs
  (_:fs) / (0:gs) = error "division by 0 attempted"
  (f:fs) / (g:gs) = let q = f/g in
       q : (fs - q.*gs) / (g:gs)

int :: Fractional a => [a] -> [a]
int fs = 0 : int1 fs 1 where
  int1 []     _ = []
  int1 (g:gs) n = g/n : (int1 gs (n+1))

expz = 1 + (int expz)

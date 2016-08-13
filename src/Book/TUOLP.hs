module Book.TUOLP

where

evens = [ x | x <- [0..], even x ]

prime :: Integer -> Bool
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False
        | otherwise = ldp n == n where
  ldp    = ldpf primes
  ldpf (p:ps) m | rem m p == 0 = p
                | p^2 > m      = m
                | otherwise    = ldpf ps m
  primes = 2 : filter prime [3..]

sieve :: [Integer] -> [Integer]
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
  where
  mark :: [Integer] -> Integer -> Integer -> [Integer]
  mark (y:ys) k m | k == m    =  0 : (mark ys  1    m)
                  | otherwise =  y : (mark ys (k+1) m)

primes :: [Integer]
primes = sieve [2..]

oddsFrom3 :: [Integer]
oddsFrom3 = 3 : map (+2) oddsFrom3

mersenne = [ (p,2^p - 1) | p <- primes, prime (2^p - 1) ]

notmersenne = [ (p,2^p - 1) | p <- primes, not (prime (2^p-1)) ]

pdivisors :: Integer -> [Integer]
pdivisors n = [ d | d <- [1..(n-1)], rem n d == 0 ]

primePairs :: [(Integer,Integer)]
primePairs = pairs primes
  where
  pairs (x:y:xys) | x + 2 == y = (x,y): pairs (y:xys)
                  | otherwise  = pairs (y:xys)

primeTriples :: [(Integer,Integer,Integer)]
primeTriples = triples primes
  where
  triples (x:y:z:xyzs)
   | x + 2 == y && y + 2 == z = (x,y,z) : triples (y:z:xyzs)
   | otherwise                = triples (y:z:xyzs)


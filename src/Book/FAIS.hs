module Book.FAIS

where

natpairs = [(x,z-x) | z <- [0..], x <- [0..z]]

rationals = [ (n,m) | (n,m) <- natpairs, m /= 0, gcd n m == 1 ]

diagonal :: (Integer -> [Bool]) -> Integer -> Bool
diagonal f n = not ((f n)!!(fromInteger n))

f :: Integer -> [Bool]
f 0 = cycle [False]
f n = True : f (n - 1)

module Book.Nats where

import Data.Ratio

data Natural = Z | S Natural deriving (Eq, Show)

instance Ord Natural where
  compare Z Z = EQ
  compare Z _ = LT
  compare _ Z = GT
  compare (S m) (S n) = compare m n

instance Enum Natural where
  succ = S
  pred Z = Z
  pred (S n) = n
  toEnum   = fromIntegral
  fromEnum = foldn succ 0
  enumFrom n = map toEnum [(fromEnum n)..]

instance Num Natural where
  (+) = foldn succ
  (*) = \m -> foldn (+m) Z
  (-) = foldn pred
  abs = id
  signum Z = Z
  signum n = (S Z)
  fromInteger n | n < 0     = error "no negative naturals"
                | n == 0    = Z
                | otherwise = S (fromInteger (n-1))

foldn :: (a -> a) -> a -> Natural -> a
foldn h c Z = c
foldn h c (S n) = h (foldn h c n)

instance Real Natural where toRational x = toInteger x % 1

instance Integral Natural where
   quotRem n d  | d > n     = (Z,n)
                | otherwise = (S q, r) where
                     (q,r) = quotRem (n-d) d
   toInteger = foldn succ 0

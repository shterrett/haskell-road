{-# LANGUAGE NoMonomorphismRestriction #-}

module Book.IAR

where

import Data.List
import Book.STAL (display)

sumOdds' :: Integer -> Integer
sumOdds' n = sum [ 2*k - 1 | k <- [1..n] ]

sumOdds :: Integer -> Integer
sumOdds n = n^2

sumEvens' :: Integer -> Integer
sumEvens' n = sum [ 2*k | k <- [1..n] ]

sumEvens :: Integer -> Integer
sumEvens n = n * (n+1)

sumInts :: Integer -> Integer
sumInts n = (n * (n+1)) `div` 2

sumSquares' :: Integer -> Integer
sumSquares' n = sum  [ k^2 |  k <- [1..n] ]

sumSquares :: Integer -> Integer
sumSquares n = (n*(n+1)*(2*n+1)) `div` 6

sumCubes' :: Integer -> Integer
sumCubes' n = sum  [ k^3 |  k <- [1..n] ]

sumCubes :: Integer -> Integer
sumCubes n = (n*(n+1) `div` 2)^2

data Natural = Z | S Natural
     deriving (Eq, Show)

plus m Z = m
plus m (S n) = S (plus m n)

m `mult` Z = Z
m `mult` (S n) = (m `mult` n) `plus` m

expn m Z = (S Z)
expn m (S n) = (expn m n) `mult` m

leq Z _ = True
leq (S _) Z = False
leq (S m) (S n) = leq m n

geq m n = leq n m
gt m n  = not (leq m n)
lt m n  = gt n m

foldn :: (a -> a) -> a -> Natural -> a
foldn h c Z = c
foldn h c (S n) = h (foldn h c n)

exclaim :: Natural -> String
exclaim = foldn ('!':) []

bittest :: [Int] -> Bool
bittest []       = True
bittest [0]      = True
bittest (1:xs)   = bittest xs
bittest (0:1:xs) = bittest xs
bittest _        = False

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' n = fib2 0 1 n where
  fib2 a b 0 = a
  fib2 a b n = fib2 b (a+b) (n-1)

data BinTree = L | N BinTree BinTree deriving Show

makeBinTree :: Integer -> BinTree
makeBinTree 0 = L
makeBinTree n = N (makeBinTree (n-1)) (makeBinTree (n-1))

count :: BinTree -> Integer
count L = 1
count (N t1 t2) = 1 + count t1 + count t2

depth :: BinTree -> Integer
depth L = 0
depth (N t1 t2) = (max (depth t1) (depth t2)) + 1

balanced :: BinTree -> Bool
balanced L = True
balanced (N t1 t2) = (balanced t1)
                  && (balanced t2)
                  && depth t1 == depth t2

data Tree = Lf | Nd Int Tree Tree deriving Show

data Tr a = Nil | T a (Tr a) (Tr a) deriving (Eq,Show)

type Dict = Tr (String,String)

split :: [a] -> ([a],a,[a])
split xs = (ys1,y,ys2)
   where
   ys1     = take n xs
   (y:ys2) = drop n xs
   n       = length xs `div` 2

data LeafTree a = Leaf a
                | Node (LeafTree a) (LeafTree a) deriving Show

ltree :: LeafTree String
ltree = Node
          (Leaf "I")
          (Node
            (Leaf "love")
            (Leaf "you"))

data Rose a = Bud a | Br [Rose a] deriving (Eq,Show)

rose = Br [Bud 1, Br [Bud 2, Bud 3, Br [Bud 4, Bud 5, Bud 6]]]

len []     = 0
len (x:xs) = 1 + len xs

cat :: [a] -> [a] -> [a]
cat []     ys = ys
cat (x:xs) ys = x : (cat xs ys)

add = foldr plus Z

mlt = foldr mult (S Z)

ln :: [a] -> Natural
ln = foldr (\ _ n -> S n) Z

rev = foldl (\ xs x -> x:xs) []

rev' = foldr (\ x xs -> xs ++ [x]) []

data Peg   = A | B | C
type Tower = ([Int], [Int], [Int])

move :: Peg  -> Peg -> Tower -> Tower
move A B (x:xs,ys,zs) =  (xs,x:ys,zs)
move B A (xs,y:ys,zs) =  (y:xs,ys,zs)
move A C (x:xs,ys,zs) =  (xs,ys,x:zs)
move C A (xs,ys,z:zs) =  (z:xs,ys,zs)
move B C (xs,y:ys,zs) =  (xs,ys,y:zs)
move C B (xs,ys,z:zs) =  (xs,z:ys,zs)

transfer :: Peg -> Peg -> Peg -> Int -> Tower -> [Tower]
transfer _ _ _ 0 tower = [tower]
transfer p q r n tower = transfer p r q (n-1) tower
                         ++
                         transfer r q p (n-1) (move p q tower')
   where tower' = last (transfer p r q (n-1) tower)

hanoi :: Int -> [Tower]
hanoi n = transfer A C B n ([1..n],[],[])

check :: Int -> Tower -> Bool
check 0 t = t == ([],[],[])
check n (xs,ys,zs)
   | xs /= [] && last xs == n = check (n-1) (init xs, zs, ys)
   | zs /= [] && last zs == n = check (n-1) (ys, xs, init zs)
   | otherwise                = False

maxT :: Tower -> Int
maxT (xs, ys, zs) =  foldr max 0 (xs ++ ys ++ zs)

checkT :: Tower -> Bool
checkT t = check (maxT t) t

parity :: Tower -> (Int,Int,Int)
parity (xs,ys,zs) = par (xs ++ [n+1], ys ++ [n],zs ++ [n+1])
  where
  n = maxT (xs, ys, zs)
  par (x:xs,y:ys,z:zs) = (mod x 2, mod y 2, mod z 2)

target :: Tower -> Peg
target t@(xs,ys,zs) | parity t == (0,1,1) = A
                    | parity t == (1,0,1) = B
                    | parity t == (1,1,0) = C

move1 :: Tower -> Tower
move1 t@(1:_,ys,zs) = move A (target t) t
move1 t@(xs,1:_,zs) = move B (target t) t
move1 t@(xs,ys,1:_) = move C (target t) t

move2 :: Tower -> Tower
move2 t@(1:xs,[],zs) = move C B t
move2 t@(1:xs,ys,[]) = move B C t
move2 t@(1:xs,ys,zs) = if ys < zs then move B C t else move C B t
move2 t@([],1:ys,zs) = move C A t
move2 t@(xs,1:ys,[]) = move A C t
move2 t@(xs,1:ys,zs) = if xs < zs then move A C t else move C A t
move2 t@([],ys,1:zs) = move B A t
move2 t@(xs,[],1:zs) = move A B t
move2 t@(xs,ys,1:zs) = if xs < ys then move A B t else move B A t

done :: Tower -> Bool
done ([],[], _) = True
done (xs,ys,zs) = False

transfer1, transfer2 :: Tower -> [Tower]
transfer1 t = t : transfer2 (move1 t)
transfer2 t = if done t then [t] else t : transfer1 (move2 t)

hanoi' :: Int -> [Tower]
hanoi' n = transfer1 ([1..n],[],[])

zazen :: [Tower]
zazen = hanoi' 64

hanoiCount :: Int -> Integer -> Tower
hanoiCount n k | k < 0        = error "argument negative"
               | k > 2^n - 1  = error "argument not in range"
               | k == 0       = ([1..n],[],[])
               | k == 2^n - 1 = ([],[],[1..n])
               | k < 2^(n-1)  = (xs ++ [n], zs, ys)
               | k >= 2^(n-1) = (ys', xs', zs' ++ [n])
              where
              (xs,ys,zs)    = hanoiCount (n-1) k
              (xs',ys',zs') = hanoiCount (n-1) (k - 2^(n-1))

toTower :: Integer -> Tower
toTower n = hanoiCount k m
   where
   n' = fromInteger (n+1)
   k  = truncate (logBase 2 n')
   m  = truncate (n' - 2^k)

data Form = P Int | Conj Form Form | Disj Form Form | Neg Form

instance Show Form where
  show (P i) = 'P':show i
  show (Conj f1 f2) = "(" ++ show f1 ++ " & " ++ show f2 ++ ")"
  show (Disj f1 f2) = "(" ++ show f1 ++ " v " ++ show f2 ++ ")"
  show (Neg f)      = "~" ++ show f

subforms :: Form -> [Form]
subforms (P n) = [(P n)]
subforms (Conj f1 f2) = (Conj f1 f2):(subforms f1 ++ subforms f2)
subforms (Disj f1 f2) = (Disj f1 f2):(subforms f1 ++ subforms f2)
subforms (Neg f) = (Neg f):(subforms f)

ccount :: Form -> Int
ccount (P n) = 0
ccount (Conj f1 f2) = 1 + (ccount f1) + (ccount f2)
ccount (Disj f1 f2) = 1 + (ccount f1) + (ccount f2)
ccount (Neg f) = 1 + (ccount f)

acount :: Form -> Int
acount (P n) = 1
acount (Conj f1 f2) = (acount f1) + (acount f2)
acount (Disj f1 f2) = (acount f1) + (acount f2)
acount (Neg f) = acount f


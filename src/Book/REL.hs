module Book.REL

where

import Data.List
import Book.SetOrd

divisors :: Integer -> [(Integer,Integer)]
divisors n = [ (d, quot n d)  | d <- [1..k], rem n d == 0 ]
   where k = floor (sqrt (fromInteger n))

prime'' :: Integer -> Bool
prime'' = \n -> divisors n == [(1,n)]

divs :: Integer -> [Integer]
divs n = [ d | d <- [1..n], rem n d == 0 ]

properDivs :: Integer -> [Integer]
properDivs n = init (divs n)

perfect :: Integer -> Bool
perfect n = sum (properDivs n) == n

type Rel a = Set (a,a)

domR :: Ord a => Rel a -> Set a
domR (Set r) = list2set [ x | (x,_) <- r ]

ranR :: Ord a => Rel a -> Set a
ranR (Set r) = list2set [ y | (_,y) <- r ]

idR :: Ord a => Set a -> Rel a
idR (Set xs) = Set [(x,x) | x <- xs]

totalR :: Set a -> Rel a
totalR (Set xs) = Set [(x,y) | x <- xs, y <- xs ]

invR :: Ord a => Rel a -> Rel a
invR (Set []) = (Set [])
invR (Set ((x,y):r)) = insertSet (y,x) (invR (Set r))

inR :: Ord a => Rel a -> (a,a) -> Bool
inR r (x,y) = inSet (x,y) r

complR :: Ord a => Set a -> Rel a -> Rel a
complR (Set xs) r =
   Set [(x,y) | x <- xs, y <- xs, not (inR r (x,y))]

reflR :: Ord a => Set a -> Rel a -> Bool
reflR set r = subSet (idR set) r

irreflR :: Ord a => Set a -> Rel a -> Bool
irreflR (Set xs) r =
   all (\ pair  -> not (inR r pair)) [(x,x) | x <- xs]

symR :: Ord a => Rel a -> Bool
symR (Set []) = True
symR (Set ((x,y):pairs)) | x == y = symR (Set pairs)
                         | otherwise =
                           inSet (y,x) (Set pairs)
                           && symR (deleteSet (y,x) (Set pairs))

transR :: Ord a => Rel a -> Bool
transR (Set []) = True
transR (Set s) = and [ trans pair (Set s) | pair <- s ] where
      trans (x,y) (Set r) =
       and [ inSet (x,v) (Set r) | (u,v) <- r, u == y ]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
(Set r) @@ (Set s) =
   Set (nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ])

repeatR :: Ord a => Rel a -> Int -> Rel a
repeatR r n | n < 1     = error "argument < 1"
            | n == 1    = r
            | otherwise = r @@ (repeatR r (n-1))

r = Set [(0,2),(0,3),(1,0),(1,3),(2,0),(2,3)]
r2 = r @@ r
r3 = repeatR r 3
r4 = repeatR r 4

s = Set [(0,0),(0,2),(0,3),(1,0),(1,2),(1,3),(2,0),(2,2),(2,3)]
test = (unionSet r (s @@ r)) == s

divides :: Integer -> Integer -> Bool
divides d n | d == 0    = error "divides: zero divisor"
            | otherwise = (rem n d) == 0

eq :: Eq a => (a,a) -> Bool
eq = uncurry (==)

lessEq :: Ord a => (a,a) -> Bool
lessEq = uncurry (<=)

inverse :: ((a,b) -> c) -> ((b,a) -> c)
inverse f (x,y) = f (y,x)

type Rel' a = a -> a -> Bool

emptyR' :: Rel' a
emptyR' = \ _ _ -> False

list2rel' :: Eq a => [(a,a)] -> Rel' a
list2rel' xys = \ x y -> elem (x,y) xys

idR' :: Eq a => [a] -> Rel' a
idR' xs = \ x y -> x == y && elem x xs

invR' :: Rel' a -> Rel' a
invR' = flip

inR' :: Rel' a -> (a,a) -> Bool
inR' = uncurry

reflR' :: [a] -> Rel' a -> Bool
reflR' xs r = and [ r x x | x <- xs ]

irreflR' :: [a] -> Rel' a -> Bool
irreflR' xs r = and [ not (r x x) | x <- xs ]

symR' :: [a] -> Rel' a -> Bool
symR' xs r = and [ not (r x y && not (r y x)) | x <- xs, y <- xs ]

transR' :: [a] -> Rel' a -> Bool
transR' xs r = and
              [ not (r x y && r y z && not (r x z))
                       | x <- xs, y <- xs, z <- xs ]

unionR' :: Rel' a -> Rel' a -> Rel' a
unionR' r s x y = r x y  || s x y

intersR' :: Rel' a -> Rel' a -> Rel' a
intersR' r s x y = r x y && s x y

reflClosure' :: Eq a => Rel' a -> Rel' a
reflClosure' r = unionR' r (==)

symClosure' :: Rel' a -> Rel' a
symClosure' r = unionR' r (invR' r)

compR' :: [a] -> Rel' a -> Rel' a -> Rel' a
compR' xs r s x y = or [ r x z && s z y | z <- xs ]

repeatR' :: [a] -> Rel' a -> Int -> Rel' a
repeatR' xs r n | n < 1     = error "argument < 1"
                | n == 1    = r
                | otherwise = compR' xs r (repeatR' xs r (n-1))

equivalenceR :: Ord a => Set a -> Rel a -> Bool
equivalenceR set r = reflR set r && symR r && transR r

equivalenceR' :: [a] -> Rel' a -> Bool
equivalenceR' xs r = reflR' xs r && symR' xs r && transR' xs r

modulo :: Integer -> Integer -> Integer -> Bool
modulo n = \ x y -> divides n (x-y)

equalSize :: [a] -> [b] -> Bool
equalSize list1 list2 = (length list1) == (length list2)

type Part = [Int]
type CmprPart = (Int,Part)

expand :: CmprPart -> Part
expand (0,p) = p
expand (n,p) = 1:(expand ((n-1),p))

nextpartition :: CmprPart -> CmprPart
nextpartition (k,(x:xs)) = pack (x-1) ((k+x),xs)

pack :: Int -> CmprPart -> CmprPart
pack 1 (m,xs) = (m,xs)
pack k (m,xs) = if k > m  then pack (k-1) (m,xs)
                else           pack k     (m-k,k:xs)

generatePs :: CmprPart -> [Part]
generatePs p@(n,[])     = [expand p]
generatePs p@(n,(x:xs)) =
      (expand p: generatePs(nextpartition p))

part :: Int -> [Part]
part n | n < 1     = error "part: argument <= 0"
       | n == 1    = [[1]]
       | otherwise = generatePs (0,[n])

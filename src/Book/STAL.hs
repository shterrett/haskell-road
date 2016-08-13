module Book.STAL

where

import Data.List
import Book.DB

naturals = [0..]

evens1 = [ n | n <- naturals , even n ]

odds1 =  [ n | n <- naturals , odd n ]

evens2 = [ 2*n | n <- naturals ]

small_squares1 = [ n^2 | n <- [0..999] ]

small_squares2 = [ n^2 | n <- naturals , n < 1000 ]

run :: Integer -> [Integer]
run n | n < 1 = error "argument not positive"
      | n == 1 = [1]
      | even n = n: run (div n 2)
      | odd n  = n: run (3*n+1)

ones = 1 : ones

characters = nub [ x    | ["play",_,_,x]  <- db ]
movies     =     [ x    | ["release",x,_] <- db ]
actors     = nub [ x    | ["play",x,_,_]  <- db ]
directors  = nub [ x    | ["direct",x,_]  <- db ]
dates      = nub [ x    | ["release",_,x] <- db ]
universe   = nub (characters++actors++directors++movies++dates)

direct     = [ (x,y)   | ["direct",x,y]  <- db ]
act        = [ (x,y)   | ["play",x,y,_]  <- db ]
play       = [ (x,y,z) | ["play",x,y,z]  <- db ]
release    = [ (x,y)   | ["release",x,y] <- db ]

charP       = \ x       -> elem x characters
actorP      = \ x       -> elem x actors
movieP      = \ x       -> elem x movies
directorP   = \ x       -> elem x directors
dateP       = \ x       -> elem x dates
actP        = \ (x,y)   -> elem (x,y) act
releaseP    = \ (x,y)   -> elem (x,y) release
directP     = \ (x,y)   -> elem (x,y) direct
playP       = \ (x,y,z) -> elem (x,y,z) play

q1 = [ x | x <- actors, directorP x ]

q2 = [ (x,y) | (x,y) <- act, directorP x ]

q3 = [ (x,y,z) | (x,y) <- direct, (y,z) <- release ]

q4 = [ (x,y,z) | (x,y) <- direct, (u,z) <- release, y == u ]

q5 = [ (x,y) | (x,y) <- direct, (u,"1995") <- release, y == u ]

q6 = [ (x,y,z) | (x,y) <- direct, (u,z) <- release,
                  y == u, z > "1995"                           ]

q7 = [ x | ("Kevin Spacey",x) <- act ]

q8 = [ x | (x,y) <- release, y > "1997", actP ("William Hurt",x) ]

q9 = q1 /= []

q10 = [ x | ("Woody Allen",x) <- direct ] /= []

q10' = directorP "Woody Allen"

elem' :: Eq a => a -> [a] -> Bool
elem' x  []                 = False
elem' x  (y:ys) | x == y    = True
                | otherwise = elem' x ys

addElem :: a -> [[a]] -> [[a]]
addElem x = map (x:)

powerList  :: [a] -> [[a]]
powerList  [] = [[]]
powerList  (x:xs) = (powerList xs) ++ (map (x:) (powerList xs))

data S = Void deriving (Eq,Show)
empty :: [S]
empty = []

display :: Int -> String -> IO ()
display n str = putStrLn (display' n 0 str)
  where
  display' _ _ [] = []
  display' n m (x:xs) | n == m    = '\n': display'  n   0  (x:xs)
                      | otherwise =  x  : display'  n (m+1)  xs


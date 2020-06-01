module Answers12 where

import Test.QuickCheck
import Data.Char

-- Exercise 1

-- a

f :: Char -> Bool
f c | not $ isAlpha c = error "non-alphabetic input"
    | otherwise       = toUpper c `elem` [ 'A'..'M' ]

test_f =
  f 'e' == True  && f 'P' == False &&
  f 'q' == False && f 'G' == True  &&
  f 'n' == False && f 'M' == True

-- b

g :: String -> Bool
g s = sum [if f c then 1 else -1 | c <- s, isAlpha c] > 0

test_g =
  g "SyzYGy" == False && g "aB7L!e"   == True &&
  g ""       == False && g "Aardvark" == True && 
  g "emnity" == False

-- c

h :: String -> Bool
h [] = False
h s  = g s 0
  where
    g :: String -> Int -> Bool
    g [] n     = n > 0
    g (x:xs) n | isAlpha x && f x       = g xs (n + 1)
               | isAlpha x && not (f x) = g xs (n - 1)
               | otherwise              = g xs n

prop_hg :: String -> Bool
prop_hg s = g s == h s

test_hg = quickCheck prop_hg


-- Exercise 2

-- a

c :: [Int] -> [Int]
c xs = [x | (x,y) <- zip xs $ tail xs, x == y]

test_c =
  c [3,1,1,3,3,5]  ==  [1,3] &&
  c [4,1,1,1,4,4]  ==  [1,1,4] &&
  c [2,2,2,2,2]    ==  [2,2,2,2] &&
  c [2,1,4,1,2]    ==  [] && 
  c [3,3,1,3,1]    ==  [3] &&
  c [42]           ==  []


-- b

d :: [Int] -> [Int]
d [] = []
d [x] = []
d (x:y:xs)
    | x == y    = x : d (y:xs)
    | otherwise = d (y:xs)

-- c

prop_cd :: [Int] -> Bool
prop_cd xs = c xs == d xs

test_cd = quickCheck prop_cd


-- test by calling main
main =
  print test_f >>
  quickCheck prop_hg >>
  quickCheck prop_cd
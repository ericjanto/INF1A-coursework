module Answers2010 where

import Test.QuickCheck
import Data.Char
import Data.List


-- Exercise 1

-- a

f :: Char -> Int
f c | elem c ['a'..'m'] = 1
    | elem c ['A'..'M'] = 3
    | elem c ['n'..'z'] = 2
    | elem c ['N'..'Z'] = 6
    | otherwise         = error "invalid input"

test_f =
  f 'a' == 1 && f 'A' == 3 &&
  f 'z' == 2 && f 'Z' == 6

-- b

g :: String -> Int
g s = sum [ f c | c <- s, isAlpha c ]

test_g =
  g "aAzZ" == 12 && g "a2m&n2z" == 6

-- c

h :: String -> Int
h [] = 0
h (c:cs) | isAlpha c = f c + h cs
         | otherwise = h cs

prop_hg :: String -> Bool
prop_hg s = g s == h s

test_gh = quickCheck prop_hg -- passes only if f does not return an error


-- Exercise 2

-- a

c :: [Int] -> Bool
c xs = and [ x > y | (x,y) <- zip xs $ tail xs ]

test_c =
  c [4,3,2,1] == True  && c [8,4,2,1,0] == True  &&
  c [2]       == True  && c [4,2,3,1]   == False &&
  c [0,1,2]   == False && c [2,2,2]     == False

-- b

d :: [Int] -> Bool
d []  = True
d [x] = True
d (x:y:xs)
    | x > y     = True && d (y:xs)
    | otherwise = False

-- c

prop_cd :: [Int] -> Bool
prop_cd xs = c xs == d xs

test_cd = quickCheck prop_cd
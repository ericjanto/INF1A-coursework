module Answers11 where

import Test.QuickCheck
import Data.Char
import Data.List

-- Exercise 1

-- a

hexaNums :: [(Char,Int)]
hexaNums = zip ['0'..'9'] [0..9]

hexaChars :: [(Char,Int)]
hexaChars = zip ['a'..'f'] [10..15]

f :: Char -> Int
f c | isDigit c = the [x | (ch,x) <- hexaNums, ch == c]
    | otherwise = the [x | (ch,x) <- hexaChars, ch == toLower c]
    where
      the [x] = x
      the _   = error "invalid input"

test_f =
  f '0' == 0  && f 'a' == 10 &&
  f '2' == 2  && f 'c' == 12 &&
  f '9' == 9  && f 'f' == 15 &&
  f 'A' == 10 && f 'C' == 12 &&
  f 'F' == 15

-- b

g :: String -> Int
g s = customMax [f c | c <- s, isDigit c || elem (toLower c) ['a'..'f']]

customMax :: [Int] -> Int 
customMax [] = -1
customMax l  = maximum l

test_g =
  g "3142" == 4 && g  "a2cz!" == 12 &&
  g ""     == -1

-- c

h :: String -> Int
h inp = maximum $ g inp
  where
    g [] = [-1]
    g (c:xs) | elem (toLower c) "abcdef0123456789" = f c : (g xs)
             | otherwise                           = g xs

prop_gh :: String -> Bool
prop_gh s = g s == h s

test_gh = quickCheck prop_gh

-- Exercise 2

-- a

c :: [Int] -> Int
c []  = error "invalid input"
c [x] = 1
c xs  = product [ x - y | (x,y) <- zip xs $ tail xs]

test_c =
  c [3,1,4,2,5] == 36  &&
  c [2,4,6,8]   == -8  &&
  c [1,2,2,1]   == 0   &&
  c [-1,2,-3,4] == 105 &&
  c [42]        == 1

-- b

d :: [Int] -> Int
d [] = error "invalid input"
d [x] = 1
d (x:y:xs) = (x-y) * d (y:xs)

test_d =
  d [3,1,4,2,5] == 36  &&
  d [2,4,6,8]   == -8  &&
  d [1,2,2,1]   == 0   &&
  d [-1,2,-3,4] == 105 &&
  d [42]        == 1

-- c

prop_cd :: [Int] -> Property
prop_cd xs = xs /= [] ==> c xs == d xs

test_cd = quickCheck prop_cd
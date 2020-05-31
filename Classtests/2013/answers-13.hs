module Answers13 where

import Test.QuickCheck
import Data.Char

-- Exercise 1

-- a

isBigHaskell :: Char -> Bool
isBigHaskell = (`elem` "HASKELL")

isLowHaskell :: Char -> Bool
isLowHaskell = (`elem` "haskell")

f :: Char -> Int
f c | isBigHaskell c = 10
    | isLowHaskell c = 5
    | isUpper c      = 2
    | isLower c      = 1
    | otherwise      = 0
  
test_f =
  f 'A' == 10 && f 'B' == 2 &&
  f '.' == 0  && f 'a' == 5 &&
  f 'b' == 1

-- b

g :: String -> Int
g s = product [f c | c <- s, isAlpha c]

test_g =
  g "aBc4E" == 100 && g "Inf1-FP" == 8 &&
  g "Java"  == 50

-- c

h :: String -> Int
h [] = 1
h (c:ss)
  | isAlpha c = f c * h ss
  | otherwise = h ss

prop_gh :: String -> Bool
prop_gh s = g s == h s

test_h = quickCheck prop_gh

-- Exercise 2

-- a

c :: String -> String -> String
c s1 s2 = [x | (x,y) <- zip s1 s2, x == y]

test_c =
  c "parallel" "mutable"  ==  "ale" && c "kangaroo" "potato" == "" &&
  c "flip" "flop"         ==  "flp" && c "Flip" "flop"       ==  "lp"

-- b

d :: String -> String -> String
d _ [] = []
d [] _ = []
d (x:xs) (y:ys)
    | x == y    = x : d xs ys
    | otherwise = d xs ys

prop_cd :: String -> String -> Bool
prop_cd s1 s2 = c s1 s2 == d s1 s2

test_d = quickCheck prop_cd

-- c

prop_cd' = prop_cd
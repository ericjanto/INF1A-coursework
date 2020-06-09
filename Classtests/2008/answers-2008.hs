module Answers2008 where

import Test.QuickCheck
import Data.List
import Data.Char
        
-- Exercise 1

-- a

f :: Char -> Int
f c | elem c "AEIOU" = 3
    | elem c "aeiou" = 2
    | isUpper c      = 2
    | isAlpha c      = 1
    | otherwise      = 0


test_f = f 'A' == 3 && f 'B' == 2 &&
         f '.' == 0 && f 'a' == 2 &&
         f 'b' == 1

-- b

g :: String -> Int
g s = product [f c | c <- s, isAlpha c]

test_g = g "aBc4E" == 12

-- c

h :: String -> Int
h [] = 1
h (x:xs) | isAlpha x = f x * h xs
         | otherwise = h xs

prop_gh :: String -> Bool
prop_gh s = g s == h s

test_gh = quickCheck prop_gh

-- Exercise 2

-- a

c :: String -> String
c s = concat [replicate i c | (c,i) <- zip s [1..]]

test_c = c "abcd" == "abbcccdddd"

-- b

d :: String -> String
d s  = e s 1
    where
      e [] _ = []
      e (x:xs) n = replicate' n x ++ e xs (n+1)
        where
          replicate' 0 c = []
          replicate' n c = c : replicate' (n-1) c

-- c

prop_cd :: String -> Bool
prop_cd s = c s == d s

test_cd = quickCheck prop_cd

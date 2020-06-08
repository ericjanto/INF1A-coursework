module Answers2009 where

import Test.QuickCheck
import Data.Char
import Data.List

-- Exercise 1

-- a

alph = ['A'..'Z']
revAlph = ['Z','Y'..'A']

pairs :: [(Char,Char)]
pairs = zip alph revAlph

f :: Char -> Char
f c | isUpper c = lookUp c pairs
    | otherwise = error "invalid input"
    where
      lookUp :: Char -> [(Char,Char)] -> Char
      lookUp c pairs = the [y | (x,y) <- pairs, x == c]


the :: [Char] -> Char
the [] = '?'
the [x] = x
the _ = error "multiple results"

test_f =
  f 'A' == 'Z' && f 'B' == 'Y' &&
  f 'M' == 'N' && f 'Z' == 'A'

-- b

g :: String -> String
g s = [f c | c <- s, isUpper c]

test_g = g "abC4Ef" == "XV"

-- c

h :: String -> String
h [] = []
h (c:xs) | isUpper c = f c : h xs
         | otherwise = h xs

prop_gh :: String -> Bool
prop_gh s = g s == h s

test_gh = quickCheck prop_gh

-- Exercise 2

-- a

c :: String -> String
c s = [c | (c,n) <- zip s [0..], even n]

test_c =
  c "abcdef" == "ace" && c [] == [] &&
  c "party"  == "pry"

-- b

d :: String -> String
d [] = []
d [x] = [x]
d (x:y:xs) = x : d xs

-- c

prop_cd :: String -> Bool
prop_cd s = c s == d s

test_cd = quickCheck prop_cd
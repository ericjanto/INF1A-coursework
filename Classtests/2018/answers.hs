module Answers2018 where

import Test.QuickCheck
import Data.Char

-- 1a)
vowels :: [Char]
vowels = ['a','e','i','o','u'] -- == "aeiou"

isVowel :: Char -> Bool
isVowel = (`elem` vowels) . toLower

test1a =
  isVowel 'e' == True  &&
  isVowel 'c' == False &&
  isVowel 'C' == False &&
  isVowel 'U' == True  &&
  isVowel '7' == False &&
  isVowel ' ' == False

--1b)
m :: String -> Int
m s = sum [if isVowel ch then 1 else -1 | ch <- s]

m' :: String -> Int
m' s =  length [ch | ch <- s, isVowel ch] - length [nv | nv <- s, not $ isVowel nv]


test1b =
  m "" == 0 &&
  m "Amoebae Are OK" == 2 &&
  m "syzygy" == -6 &&
  m "Haskell rules!" == -6 &&
  m "cafe au lait" == 0 &&
  m "aquaria" == 3


-- 1c)
n :: String -> Int
n [] = 0
n (ch:ss)
    | isVowel ch = n ss + 1
    | otherwise  = n ss - 1


prop_mn :: String -> Bool
prop_mn s = m s == n s

test2b = quickCheck prop_mn

-- 2a)
f :: String -> Bool
f s = and [alternates x y | (x,y) <- zip s $ tail s]

alternates :: Char -> Char -> Bool
alternates x y = isAlpha x /= isAlpha y

test2a =
  f "" == True &&
  f "Oops" == False &&
  f ".I-n-F1A" == True &&
  f "I O U" == True &&
  f "O" == True &&
  f "..-. .--." == False

-- 2b)
g :: String -> Bool
g []  = True
g [x] = True
g (x:y:xs)
  | alternates x y = g (y:xs)
  | otherwise      = False


-- 2c)
prop_fg :: String -> Bool
prop_fg s = f s == g s

test2c = quickCheck prop_fg
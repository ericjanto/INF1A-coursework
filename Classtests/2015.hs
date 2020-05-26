module Classtest2015 where

import Test.QuickCheck
import Data.List
import Data.Char

-- ex 1

count :: String -> Int
count xs = sum[1 | x <- xs, isDigit x || isUpper x]

countRec :: String -> Int
countRec [] = 0
countRec (x:xs)
    | isUpper x || isDigit x = 1 + countRec xs
    | otherwise              = countRec xs


prop_count :: String -> Bool
prop_count xs = count xs == countRec xs

-- ex 2

isNext :: Int -> Int -> Bool
isNext x y = next x == y
  where next :: Int -> Int
        next x
          | even x = x `div` 2
          | odd  x = x * 3 + 1 


collatz :: [Int] -> Bool
collatz xs = and [isNext x y | (x,y) <- zip xs (tail xs)]


collatzRec :: [Int] -> Bool
collatzRec []  = True
collatzRec [x] = True
collatzRec (x:y:xs)
  | isNext x y = True && collatzRec (y:xs)
  | otherwise  = False


prop_collatz :: [Int] -> Bool
prop_collatz xs = collatz xs == collatzRec xs
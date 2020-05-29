module Classtest16 where

import Test.QuickCheck
import Data.Char
import Data.List

-- 1a)
vownums :: [Int]
vownums = [1,8,11,18] ++ [80..89]

vowelly :: Int -> Bool
vowelly = (`elem` vownums)

-- 1b)
count :: [Int] -> Int
count = (\xs -> length $ filter vowelly xs)

-- 1c)
countRec :: [Int] -> Int
countRec [] = 0
countRec (x:xs)
    | vowelly x = 1 + countRec xs
    | otherwise = countRec xs

-- Additional property check for 1b) && c)
prop_count :: [Int] -> Bool
prop_count xs = count xs == countRec xs

-- 2a)
c :: Char -> String -> String
c ch s = [f char i| (char,i) <- zip s [0..]]
    where
      f char i = if even i then ch else char

-- 2b)
d :: Char -> String -> String
d ch s = e ch s 0
    where
      e :: Char -> String -> Int -> String
      e _ [] _ = []
      e ch (x:xs) n
        | even n = ch : e ch xs (n+1)
        | odd n  = x : e ch xs (n+1)

-- 2c)
prop_replace :: Char -> String -> Bool
prop_replace ch s = c ch s == d ch s
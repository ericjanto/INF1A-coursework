-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 2
--
-- Week 2(23-27 Sep.)

module Tutorial2 where

import Data.Char
import Data.List
import Test.QuickCheck
import Control.Monad (guard)


-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, even x]

-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)


-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensReference xs


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x <= hi, x >= lo]


-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = sum[1 | x <- list, x > 0]

countPositives' :: [Int] -> Int
countPositives' list = length[x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives list = countPositives list == countPositives' list


-- 4. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = product[digitToInt s | s <- str, isDigit s]

countDigits :: String -> Int
countDigits str = length[digitToInt s | s <- str, isDigit s]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs <= 9^x
               where x = countDigits xs


-- 5. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = toUpper x : [toLower s | s <- xs]


-- 6. title

lowercase :: String -> String
lowercase str = [toLower s | s <- str]

-- List-comprehension version
title :: [String] -> [String]
title [] = []
title (s:strs) = capitalise s : [if length str > 3 then capitalise str else lowercase str | str <- strs]


-- 7. signs

sign :: Int -> Char
sign x
    | x >= 1 && x <= 9   = '+'
    | x == 0             = '0'
    | x <= -1 && x >= -9 = '-'
    | otherwise          = error "Broo I didn't expected such an input:("

signs :: [Int] -> String
signs xs = [sign x | x <- xs]


-- 8. score

vowels :: [Char]
vowels = ['a','e','i','o','u']

score :: Char -> Int
score x
    | not $ isAlpha x        = 0
    | isVowel x && isUpper x = 3
    | isVowel x || isUpper x = 2
    | otherwise              = 1
    where
      isVowel :: Char -> Bool
      isVowel c = toLower c `elem` vowels

totalScore :: String -> Int
totalScore xs = product[score x | x <- xs, isAlpha x]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = totalScore xs >= 1

-- Tutorial Activity
-- 10. pennypincher

discount :: Int -> Int
discount x = round $ fromIntegral x * 0.9

-- List-comprehension version.
pennypincher :: [Int] -> Int
pennypincher prices = sum[discount x | x <- prices, discount x <= 19900]

-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs <= sum[x | x <- xs, x > 0]

-- Optional Material

-- 11. crosswordFind

lookUp :: Char -> Int -> String -> Bool
lookUp letter inPosition w = w !! inPosition == letter

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [w | w <- words, length w == len, lookUp letter pos w]


-- 12. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str goal = [b | (a,b) <- str `zip` [0..], a == goal]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = length (search str goal) <= length str


-- 13. contains

contains :: String -> String -> Bool
contains str substr = or [substr `isPrefixOf` sub | sub <- subs str]
                where
                  subs :: String -> [String]
                  subs str = [drop n str | n <- [0..length str]]


-- Depending on the property you want to test, you might want to change the type signature
prop_contains :: String -> Bool
prop_contains [] = True
prop_contains str@(x:xs) = str `contains` xs
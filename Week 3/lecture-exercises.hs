module Practice where

import Test.QuickCheck
import Data.Char



-- Recursive
search :: Eq a => [a] -> a -> [Int]
search xs y = srch xs y 0
    where srch :: Eq a => [a] -> a -> Int -> [Int]
          srch [] y n = []
          srch (x:xs) y n
              | x == y = n : srch xs y (n+1)
              | otherwise = srch xs y (n+1)

--  Comprehensive
search' :: Eq a => [a] -> a -> [Int]
search' xs y = [ i | (x,i) <- xs `zip` [0..], x == y]

-- Test
prop_search :: Eq a => [a] -> a -> Bool
prop_search xs y = search xs y == search' xs y


-- a)
f :: Char -> Bool
f ch = ch `elem` ['g','j','p','q','y']

-- b)
g :: String -> Int
g s = length [ch | ch <- s, f ch]

-- c)
h :: String -> Int
h []     = 0
h (x:xs)
    | f x       = 1 + h xs
    | otherwise = h xs


-- a)
c :: String -> String
c s = [if even i then toUpper ch else ch | (ch,i) <- s `zip` [0..]]

-- b)
d :: String -> String
d s = e s 0
    where
        e :: String -> Int -> String
        e [] _     = []
        e (s:ss) n
            | even n    = toUpper s : e ss (n+1)
            | otherwise = s : e ss (n+1)

-- c)
prop_cd :: String -> Bool
prop_cd s = c s == d s
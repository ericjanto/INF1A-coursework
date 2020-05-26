module LibraryFunctions where

import Test.QuickCheck
import Data.Char
import Data.List

replicate' :: Int -> a -> [a]
replicate' x c 
    | x <= 0    = []
    | otherwise = c : replicate' (x-1) c 
    
prop_replicate :: (Eq a) => Int -> a -> Bool
prop_replicate x c = replicate x c == replicate' x c

--
take' :: Int -> [a] -> [a]
take' _ []      = []
take' n (x:xs)
    | n <= 0    = []
    | otherwise = x : take' (n-1) xs

prop_take :: (Eq a) => Int -> [a] -> Bool
prop_take n x = take' n x == take n x

-- 
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x] 

prop_reverse :: (Eq a) => [a] -> Bool
prop_reverse xs = reverse' xs == reverse xs

--
repeat' :: a -> [a]
repeat' x = x : repeat' x

prop_repeat :: (Eq a) => a -> Bool
prop_repeat x = take 100 (repeat' x) == take 100 (repeat x)

--
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (l:ls) = (x,l) : zip' xs ls

prop_zip :: (Eq a, Eq b) => [a] -> [b] -> Bool
prop_zip xs ls = zip' xs ls == zip xs ls

--
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
    | x == y    = True
    | otherwise = elem' x ys

prop_elem :: (Eq a) => a -> [a] -> Bool
prop_elem x xs = elem' x xs == elem x xs
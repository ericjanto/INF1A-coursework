-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 3
--
-- Week 3(30-04 Oct.)
module Tutorial3 where

import Data.Char
import Data.List
import Test.QuickCheck

import Data.Function
import Data.Maybe


-- 1.

halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs)
      | even x    = x `div` 2 : halveEvensRec xs
      | otherwise = halveEvensRec xs

halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs


-- 2.

inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec _ _ [] = []
inRangeRec m n (x:xs)
      | x >= m && x <= n = x : inRangeRec m n xs
      | otherwise        = inRangeRec m n xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]

prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs


-- 3.

countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs)
      | x > 0     = 1 + countPositivesRec xs
      | otherwise = countPositivesRec xs

countPositives :: [Int] -> Int
countPositives list = length [x | x <- list, x > 0]

prop_countPositives :: [Int] -> Bool
prop_countPositives l = countPositives l == countPositivesRec l


-- 4.

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (x:xs)
      | isDigit x = digitToInt x * multDigitsRec xs
      | otherwise = multDigitsRec xs

multDigits :: String -> Int
multDigits str = product [digitToInt ch | ch <- str, isDigit ch]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs


-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.

rotate :: Int -> [Char] -> [Char]
rotate k list | 0 <= k && k <= length list = drop k list ++ take k list
              | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

alphabet = ['A'..'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================


-- 5.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch xs = check [y | (x,y) <- xs, ch == x]
            where check [] = ch
                  check xs = head xs

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch [] = ch
lookUpRec ch ((x,y):xs)
      | ch == x   = y
      | otherwise = lookUpRec ch xs

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = lookUp c k == lookUpRec c k 


-- 6.

encipher :: Int -> Char -> Char
encipher k ch = lookUp ch (makeKey k)


-- 7.

normalize :: String -> String
normalize s = [toUpper ch | ch <- s, isDigit ch || toUpper ch `elem` alphabet]


encipherStr :: Int -> String -> String
encipherStr k str = map (encipher k) (normalize str)


-- 8.

reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [(y,x) | (x,y) <- xs]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec []         = []
reverseKeyRec ((x,y):xs) = (y,x) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xs = reverseKey xs == reverseKeyRec xs


-- 9.

decipher :: Int -> Char -> Char
decipher k ch = lookUp ch (reverseKey (makeKey k))

decipherStr :: Int -> String -> String
decipherStr k str = map (decipher k) (normalize str)

-- Optional Material
-- =================


-- 10.

contains :: String -> String -> Bool
contains str sub = or[sub `isPrefixOf` suffix | suffix <- sufs str]
      where sufs :: String -> [String]
            sufs str = [ [sub | sub <- drop n str ] | n <- [0..length str] ]

containsRec :: String -> String -> Bool
containsRec _ [] = True
containsRec [] _  = False
containsRec str@(x:xs) sub
      | sub == str = True
      | otherwise  = containsRec xs sub


prop_contains :: String -> String -> Bool
prop_contains str sub = contains str sub == containsRec str sub


-- 11.

candidates :: String -> [(Int, String)]
candidates str = [pair | pair <- cnds, check pair]
      where cnds :: [(Int,String)]
            cnds = [(n,decipherStr n str) | n <- [0..25]]
            
check :: (Int, String) -> Bool
check (_,str) = contains str "THE" || contains str "AND"

candidatesRec :: String -> [(Int, String)]
candidatesRec str = cnds str 0
      where cnds :: String -> Int -> [(Int,String)]
            cnds [] _ = []
            cnds str n
                  | n > 25                      = []
                  | check (n,decipherStr n str) = (n,decipherStr n str) : cnds str (n+1)
                  | otherwise                   = cnds str (n+1)

prop_candidates :: String -> Bool
prop_candidates str = candidates str == candidatesRec str

-- 12.

splitEachFive :: String -> [String]
splitEachFive str
      | checkLn str = take 5 str : splitEachFive (drop 5 str)
      | null str    = []
      | otherwise   = [fillUp str]
      where
        checkLn :: String -> Bool
        checkLn str = length str >= 5 
        fillUp :: String -> String
        fillUp str = str ++ take (5 - length str) (cycle "X")

prop_transpose :: String -> Bool
prop_transpose str = transpose (transpose (splitEachFive str)) == splitEachFive str

counterExample :: Bool
counterExample = transpose (transpose ["1","22","333"]) /= ["1","22","333"]


-- 13.
encrypt :: Int -> String -> String
encrypt k str = concat (transpose (splitEachFive (decipherStr k str)))


splitFiveWays :: String -> [String]
splitFiveWays xs | n `mod` 5 == 0 = splitEach (n `div` 5) xs
                 | otherwise      = error "splitFiveWays: not a multiple of 5"
                 where n = length xs

splitEach :: Int -> String -> [String]
splitEach _ [] = []
splitEach n xs = take n xs : splitEach n (drop n xs)

-- 14.
decrypt :: Int -> String -> String
decrypt n str = concat (transpose (splitFiveWays (decipherStr n str)))
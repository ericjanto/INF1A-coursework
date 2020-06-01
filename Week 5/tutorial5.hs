-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 5
--
-- Week 5(14-18 Oct.)

module Tutorial5 where

import Data.Char
import Data.Ratio
import Test.QuickCheck

-- 1. Map

-- a.
doubles :: [Int] -> [Int]
doubles = map (*2)

test_doubles =
  doubles [1,2,3] == [2,4,6] &&
  doubles []      == []

-- b.        
penceToPounds :: [Int] -> [Float]
penceToPounds = map (/100) . map fromIntegral

test_pence =
  penceToPounds [] == [] &&
  penceToPounds [120,240,0] == [1.2,2.4,0]

-- c.
uppersComp :: String -> String
uppersComp = map toUpper

test_uppersComp =
  uppersComp []       == [] &&
  uppersComp "qwertz" == "QWERTZ"

-- 2. Filter
-- a.
alphas :: String -> String
alphas = filter isAlpha

alphas' xs = [ x | x <- xs, isAlpha x ]

prop_alphas :: String -> Bool
prop_alphas s = alphas s == alphas' s

test_alphas = quickCheck prop_alphas

-- b.
above :: Int -> [Int] -> [Int]
above x = filter (>x)

above' x xs = [ y | y <- xs, y > x]

prop_above :: Int -> [Int] -> Bool
prop_above x xs = above x xs == above' x xs

test_above = quickCheck prop_above

-- c.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter unequal
  where
    unequal (x,y) = x == y

unequals' :: [(Int,Int)] -> [(Int,Int)]
unequals' ps = [ pair | pair@(x,y) <- ps, x == y ]

prop_unequals :: [(Int,Int)] -> Bool
prop_unequals ps = unequals ps == unequals' ps

test_unequals = quickCheck prop_unequals

-- d.
rmCharComp :: Char -> String -> String
rmCharComp ch = filter (/= ch)

rmCharComp' ch s = [c | c <- s, c /= ch]

prop_rmCharComp :: Char -> String -> Bool
prop_rmCharComp ch s = rmCharComp ch s == rmCharComp' ch s

test_rmCharComp = quickCheck prop_rmCharComp

-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' = map (*2) . filter (>3)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

test_largeDoubles = quickCheck prop_largeDoubles

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' = map reverse . filter evenL
  where
    evenL s = even $ length s

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs

test_reverseEven = quickCheck prop_reverseEven

-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold = foldr (&&) True

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

test_and = quickCheck prop_and

-- b.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (l:ls) = l ++ concatRec ls

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

test_concat = quickCheck prop_concat

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec [] y = y
rmCharsRec _ [] = []
rmCharsRec (x:xs) y = rmCharsRec xs (rmCharComp x y)

rmCharsFold :: String -> String -> String
rmCharsFold chs str = foldr rmCharComp str chs

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str

test_rmChars = quickCheck prop_rmChars


type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform = undefined

-- b.
valid :: Matrix -> Bool
valid = undefined


-- 6.
matrixWidth :: Matrix -> Int
matrixWidth m = undefined

matrixHeight :: Matrix -> Int
matrixHeight m = undefined

plusM :: Matrix -> Matrix -> Matrix
plusM = undefined

-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM = undefined

-- 8.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = undefined

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = undefined

-- -----------------------------------
-- -----------------------------------
-- -- Optional material
-- -----------------------------------
-- -----------------------------------
-- -- 9.

-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined
        
determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 :: Rational -> Rational -> Rational 
                -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)
        
prop_inverse3 :: Triple Rational -> 
                 Triple Rational -> 
                 Triple Rational ->
                 Property
prop_inverse3 r1 r2 r3 = undefined
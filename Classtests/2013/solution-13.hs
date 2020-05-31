-- Informatics 1 - Functional Programming 
-- Class Test 2013
--
-- Sample solutions

-- I would not expect you to include the tests and use of QuickCheck
-- shown below in this exam, except for question 2(c) which explicitly
-- asks for a QuickCheck property, since you have no access to Haskell.
-- But this style of testing is *highly recommended* when you do have
-- Haskell access, for instance for tutorial exercises and the final exam.

import Data.Char
import Test.QuickCheck

-- Problem 1

-- a

f :: Char -> Int
f x | inHaskell x  =  upperBonus x 5
    | isAlpha x    =  upperBonus x 1
    | otherwise    =  0
   where
     inHaskell x  =  toLower x `elem` "haskell"
     upperBonus x n | isUpper x  =  n*2
                    | otherwise  =  n

test_1a  =
    f 'A'  ==  10   &&   f 'B'  ==   2   &&   f '.'  ==  0 &&
    f 'a'  ==  5    &&   f 'b'  ==   1

-- Here's another solution that gives the same result

f' :: Char -> Int
f' x | x `elem` "haskell" = 5
     | x `elem` "HASKELL" = 10
     | isLower x = 1
     | isUpper x = 2
     | otherwise = 0

prop_ff' x  =  f x == f' x
check_1a = quickCheck prop_ff'

-- b

g :: String -> Int
g xs = product [ f x | x <- xs, isAlpha x ]

test_1b =
    g "aBc4E"  ==  100   &&   g "Inf1-FP"  ==   8   &&   g "Java"  ==   50

-- c

h :: String -> Int
h []  =  1
h (x:xs) | isAlpha x  =  f x * h xs
         | otherwise  =  h xs

test_1c =
    h "aBc4E"  ==  100   &&   h "Inf1-FP"  ==   8   &&   h "Java"  ==   50

test_1 = test_1a && test_1b && test_1c

prop_gh xs  =  g xs == h xs
check_1 = quickCheck prop_gh

-- Problem 2

-- a

c :: String -> String -> String
c xs ys = [ x | (x,y) <- zip xs ys, x==y ]

test_2a  =
    c "flip" "flop"         ==  "flp" && c "Flip" "flop"        ==  "lp" &&
    c "parallel" "mutable"  ==  "ale" && c "kangaroo" "potato"  ==  ""

-- b

d :: String -> String -> String
d [] ys = []
d xs [] = []
d (x:xs) (y:ys) | x==y      = x : d xs ys
                | otherwise = d xs ys

test_2b  =
    d "flip" "flop"         ==  "flp" && d "Flip" "flop"        ==  "lp" &&
    d "parallel" "mutable"  ==  "ale" && d "kangaroo" "potato"  ==  ""

-- c

prop_cd :: String -> String -> Bool
prop_cd xs ys  =  c xs ys == d xs ys

test_2 = test_2a && test_2b

check_2 = quickCheck prop_cd
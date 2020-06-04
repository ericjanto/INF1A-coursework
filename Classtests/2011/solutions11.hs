-- Informatics 1 - Functional Programming 
-- Class Exam
--
-- Solutions

import Data.Char
import Test.QuickCheck

-- Problem 1.

--a
f :: Char -> Int
f x | '0' <= x && x <= '9'  =  ord x - ord '0'
    | 'a' <= x && x <= 'f'  =  ord x - ord 'a' + 10
    | 'A' <= x && x <= 'F'  =  ord x - ord 'A' + 10

isHex :: Char -> Bool
isHex x  =  
       '0' <= x && x <= '9'
    || 'a' <= x && x <= 'f'
    || 'A' <= x && x <= 'F'

test_f f =
    f '0'  ==  0  &&  f 'a'  ==   10  &&  f 'A'  ==   10  &&
    f '2'  ==  2  &&  f 'c'  ==   12  &&  f 'C'  ==   12  &&
    f '9'  ==  9  &&  f 'f'  ==   15  &&  f 'F'  ==   15

test_1a  =  test_f f

--a, alternative solution

f' :: Char -> Int
f' x = lookUp key (toLower x) 

isHex' :: Char -> Bool
isHex' x  =  toLower x `elem` map fst key

key :: [(Char,Int)]
key =  zip (['0'..'9']++['a'..'f']) [0..15]

lookUp :: Eq a => [(a,b)] -> a -> b
lookUp xys x  =  the [ y | (x',y) <- xys, x == x' ]
  where
  the [x] = x
  -- will signal an error if value not in list

test_1a'  =  test_f f'

--b
g :: String -> Int
g xs = maximum (-1 : [ f x | x <- xs, isHex x ])

test_g g = 
   g  "3142" == 4  &&  g "a2cz!" == 12  &&  g "" == -1

test_1b = test_g g 

--b, alternative solution
g' :: String -> Int
g' xs = myMax [ f x | x <- xs, isHex x ]
  where
  myMax []  =  -1
  myMax xs  =  maximum xs

test_1b'  =  test_g g'
prop_1b' xs  =  g xs == g' xs

--c
h :: String -> Int
h [] = -1
h (x:xs) | isHex x       =  f x `max` h xs
         | otherwise     =  h xs

test_1c  =  test_g h
prop_1c xs  =  g xs == h xs

--c, alternative solution
h' :: String -> Int
h' xs  =  k xs (-1)
  where
  k [] y                  =  y
  k (x:xs) y | isHex x    =  k xs (f x `max` y)
             | otherwise  =  k xs y

test_1c'  =  test_g h'
prop_1c' xs  =  h xs == h' xs

test_1 = test_1a && test_1a' && test_1b && test_1b' && test_1c && test_1c'

-- Problem 2.

--a
c :: [Int] -> Int
c (x:xs)  =  product [ u-v | (u,v) <- zip (x:xs) xs ]

test_c c =
    c [3,1,4,2,5]  ==  36   &&
    c [2,4,6,8]    ==  -8   &&
    c [1,2,2,1]    ==  0    &&
    c [-1,2,-3,4]  ==  105  &&
    c [42]         ==  1

test_2a = test_c c

--b
d :: [Int] -> Int
d [x]       =  1
d (x:y:zs)  =  (x-y) * d (y:zs)

test_2b =  test_c d

--c

prop_cd :: [Int] -> Bool
prop_cd xs  =  null xs || c xs == d xs

test_2 = test_2a && test_2b
test = test_1 && test_2

main =
  print test >>
  quickCheck prop_1b' >>
  quickCheck prop_1c  >>
  quickCheck prop_1c' >>
  quickCheck prop_cd
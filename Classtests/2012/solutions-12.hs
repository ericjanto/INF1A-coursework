-- Informatics 1 - Functional Programming 
-- Class Test 2012
--
-- Sample solutions

-- I would not expect you to include the tests and use of QuickCheck
-- shown below in this exam, except for question 2(c) which explicitly
-- asks for a QuickCheck property, since you have no access to Haskell.
-- But this style of testing is highly recommended when you do have
-- Haskell access, for instance for tutorial exercises and the final exam.

import Data.Char
import Test.QuickCheck

-- Problem 1

-- a

f :: Char -> Bool
f x | 'a' <= x && x <= 'm'  =  True
    | 'A' <= x && x <= 'M'  =  True
    | 'n' <= x && x <= 'z'  =  False
    | 'N' <= x && x <= 'Z'  =  False

test_f f =
    f 'e'  ==  True && f 'P'  ==   False && f 'q'  ==   False &&
    f 'G'  ==  True && f 'n'  ==   False && f 'M'  ==   True

test_1a  =  test_f f

-- b

g :: String -> Bool
g xs = length [ x | x <- xs, isAlpha x, f x ]
         > length [ x | x <- xs, isAlpha x, not(f x) ]

test_g g = 
    g "SyzYGy"  ==  False  && g "aB7L!e"  ==  True  && g ""  ==  False &&
    g "Aardvark"  ==  True && g "emnity"  ==  False

test_1b = test_g g 

-- c

h :: String -> Bool
h xs = count xs > 0
         where
         count []                             =  0
         count (x:xs) | isAlpha x && f x       =  count xs + 1
                      | isAlpha x && not(f x)  =  count xs - 1
                      | otherwise              =  count xs

test_1c  =  test_g h
prop_1c xs  =  all isAscii xs ==> g xs == h xs
-- Note: pre-condition isAscii xs is required because isAlpha x is also
-- True for non-ASCII Unicode alphabetic characters, and f is only
-- defined for ASCII.  (I would not expect you to know this, and I would
-- not include this question on the final exam because of the chance that
-- you might waste time struggling with QuickCheck before figuring it out.)

-- c, alternative solution

h' :: String -> Bool
h' xs = countFirst xs > countLast xs
         where
         countFirst []                        =  0
         countFirst (x:xs) | isAlpha x && f x  =  countFirst xs + 1
                           | otherwise         =  countFirst xs
         countLast []                             =  0
         countLast (x:xs) | isAlpha x && not(f x)  =  countLast xs + 1
                          | otherwise              =  countLast xs

test_1c'  =  test_g h'
prop_1c' xs  =   all isAscii xs ==> h xs == h' xs

test_1 = test_1a && test_1b && test_1c && test_1c'

-- Problem 2

-- a

c :: [Int] -> [Int]
c (x:xs)  =  [ x | (x,y) <- zip (x:xs) xs, x == y ]

test_c c =
    c [3,1,1,3,3,5]  ==  [1,3]     && c [2,1,4,1,2]  ==  [] &&
    c [4,1,1,1,4,4]  ==  [1,1,4]   && c [3,3,1,3,1]  ==  [3] &&
    c [2,2,2,2,2]    ==  [2,2,2,2] && c [42]         ==  []

test_2a = test_c c

-- b

d :: [Int] -> [Int]
d [x]       =  []
d (x:y:zs) | x==y       = x : d (y:zs)
           | otherwise  = d (y:zs)

test_2b =  test_c d

-- c

prop_cd :: [Int] -> Bool
prop_cd xs  =  null xs || c xs == d xs

test_2 = test_2a && test_2b

-- testing everything

test = test_1 && test_2

main =
  print test >>
  quickCheck prop_1c  >>
  quickCheck prop_1c' >>
  quickCheck prop_cd
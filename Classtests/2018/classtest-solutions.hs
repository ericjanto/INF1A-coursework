-- Informatics 1 - Functional Programming 
-- Class Test 2018
--
-- Sample solutions

-- I would not expect you to include the tests and use of QuickCheck
-- shown below, except for question 2(c) which explicitly asks for a
-- QuickCheck property, since you have no access to Haskell.
-- But this style of testing is *highly recommended* when you do have
-- Haskell access, for instance for tutorial exercises and the final exam.

import Data.Char
import Test.QuickCheck

-- Problem 1

-- a

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiouAEIOU"

test_isVowel =
    isVowel 'e' == True  && isVowel 'U' == True  &&
    isVowel 'c' == False && isVowel '7' == False &&
    isVowel 'C' == False && isVowel ' ' == False

-- b

m :: String -> Int
m cs = (length [ c | c <- cs, isVowel c ]) - (length [ c | c <- cs, not (isVowel c) ])

test_m =
    m ""             == 0  && m "Amoebae Are OK" == 2  &&
    m "syzygy"       == -6 && m "Haskell rules!" == -6 &&
    m "cafe au lait" == 0  && m "aquaria"        == 3

-- Here are two alternative ways of calculating the same thing, since length = vowels + non-vowels

m' :: String -> Int
m' cs = (length cs) - 2 * (length [ c | c <- cs, not (isVowel c) ])

m'' :: String -> Int
m'' cs = 2 * (length [ c | c <- cs, isVowel c ]) - (length cs)

prop_m :: String -> Bool
prop_m cs = m cs == m' cs && m' cs == m'' cs

-- c

n :: String -> Int
n [] = 0
n (c:cs) | isVowel c = (n cs) + 1
         | otherwise = (n cs) - 1

test_n =
    n ""             == 0  && n "Amoebae Are OK" == 2  &&
    n "syzygy"       == -6 && n "Haskell rules!" == -6 &&
    n "cafe au lait" == 0  && n "aquaria"        == 3

prop_mn :: String -> Bool
prop_mn cs = m cs == n cs

-- Problem 2

-- a

f :: String -> Bool
f [] = True
f (c:cs) = and [ isAlpha a /= isAlpha b | (a,b) <- zip (c:cs) cs ]

test_f =
  f ""         == True && f "Oops"      == False  &&
  f ".I-n-F1A" == True && f "I O U"     == True   &&
  f "O"        == True && f "..-. .--." == False

-- b

g :: String -> Bool
g [] = True
g [a] = True
g (a:b:cs) | isAlpha a /= isAlpha b = g (b:cs)
           | otherwise              = False

test_g =
  g ""         == True && g "Oops"      == False  &&
  g ".I-n-F1A" == True && g "I O U"     == True   &&
  g "O"        == True && g "..-. .--." == False

-- c

prop_fg :: String -> Bool
prop_fg s = f s == g s

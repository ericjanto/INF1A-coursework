-- Solutions to class exam 2008

import Char
import Test.QuickCheck

-- 1.

-- 1a.

-- solution 1:
f :: Char -> Int
f x  =  s (isAlpha x) + s (isUpper x) + s (elem (toLower x) "aeiou")
  where
  s t  =  if t then 1 else 0 

-- solution 2:
f2 :: Char -> Int
f2 x | isUpper x && isVowel x  =  3
     | isUpper x || isVowel x  =  2
     | isAlpha x               =  1
     | otherwise               =  0
    where
      isVowel c  =  c `elem` "AEIOUaeiou"

prop_f  :: Bool
prop_f  =  f 'A' == 3 && f 'a' == 2 && f 'B' == 2 && f 'b' == 1 && f '.' == 0

prop_f1f2 :: Char -> Bool
prop_f1f2 x  =  f x == f2 x


-- 1b.

g :: String -> Int
g xs  =  product [ f x | x <- xs, isAlpha x ]

prop_g  = g "aBc4E" == 12


-- 1c.

h :: String -> Int
h []  =  1
h (x:xs) | isAlpha x  =  f x * h xs
         | otherwise  =  h xs

prop_h  = h "aBc4E" == 12
prop_gh xs  =  g xs == h xs



-- 2.

-- 2a.

c :: String -> String
c xs  =  concat [ replicate i x | (i,x) <- zip [1..] xs ]


-- 2b.

-- solution 1
d :: String -> String
d xs  =  a 1 xs
  where
  a n []     =  []
  a n (x:xs) =  b x n ++ a (n+1) xs
    where
    b x 0      =  []
    b x (m+1)  =  x : b x m 

-- solution 2
d2 :: String -> String
d2 xs  =  e 1 0 xs
  where
  e i n []  =  []
  e i n (c:str) | i <= n     =  e (i+1) 0 str
                | otherwise  =  c : e i (n+1) (c:str)

prop_d1d2 :: String -> Bool
prop_d1d2 str  =  d str == d2 str


-- 2c.

prop_cd :: String -> Bool
prop_cd xs  =  c xs == d xs


test_all = quickCheck prop_f >>
           quickCheck prop_f1f2 >>
           quickCheck prop_g >>
           quickCheck prop_h >>
           quickCheck prop_gh >>
           quickCheck prop_d1d2 >>
           quickCheck prop_cd
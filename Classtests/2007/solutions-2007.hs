-- Solutions to class exam

import Char

-- 1.

-- 1a.

f :: Char -> Int
f x | 'a' <= x && x <= 'z'  =  ord x - ord 'a'
    | 'A' <= x && x <= 'Z'  =  ord x - ord 'A'

x1a = f 'A' == 0 && f 'B' == 1 && f 'Z' == 25 &&
      f 'a' == 0 && f 'b' == 1 && f 'z' == 25

-- 1b.

g :: String -> Int
g xs  =  sum [ f x | x <- xs, isAlpha x ]

x1b = g "aBc4e" == 7 && g "?!" == 0

-- 1c.

h :: String -> Int
h []  =  0
h (x:xs) | isAlpha x  =  f x + h xs
         | otherwise  =  h xs

x1c = h "aBc4e" == 7 && h "?!" == 0

-- 2.

-- 2a.

c :: [Int] -> [Int] -> [Int]
c xs ys | length xs == length ys  =  [ x-y | (x,y) <- zip xs ys ]

x2a = c [5,7,3] [1,2,4] == [4,5,-1]

-- 2b.

d :: [Int] -> [Int] -> [Int]
d [] []          =  []
d (x:xs) (y:ys)  =  x-y : d xs ys

x2b = d [5,7,3] [1,2,4] == [4,5,-1]

-- 2c.

e :: [Int] -> [Int] -> Bool
e xs ys  =  and [ z == 0 | z <- c xs ys ]

x2c = e [3,3,3] [3,3,3] && not (e [3,3,3] [3,3,2]) && e [] []

ok = x1a && x1b && x1c && x2a && x2b && x2c

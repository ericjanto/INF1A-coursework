-- Informatics 1 - Functional Programming
-- Class exam 2009
--
-- Solutions

import Data.Char
import Test.QuickCheck

-- 1a.

f x | isUpper x  =  chr (ord 'Z' - (ord x - ord 'A'))
    | otherwise  =  error "bad call to f: argument is not upper case"

-- Alternative solutions

f2 x | isUpper x  =  chr (ord 'A' + ord 'Z' - ord x)

f3 x  =  lookUp key x
    where
      key  =  zip ['A'..'Z'] (reverse ['A'..'Z'])
      lookUp xys x  =  the [ y | (x',y) <- xys, x == x' ]
          where
            the [x]  =  x

-- Note that ['Z'..'A'] does not give the reverse of ['A'..'Z'],
--  but ['Z','Y'..'A'] does!


-- 1b.

g xs  =  [ f x | x <- xs, isUpper x ]

-- 1c.

h []                  =  []
h (x:xs) | isUpper x  =  f x : h xs
         | otherwise  =  h xs



-- 2a. 

c xs  =  [ x | (x,i) <- zip xs [0..], even i ]

-- Alternative solutions

c2 xs  =  [ xs!!i | i <- [0..length xs-1], even i ]

c3 xs  =  [ xs!!i | i <- [0,2..length xs-1] ]


-- 2b.


d []        =  []
d [x]       =  [x]
d (x:y:xs)  =  x : d xs

-- Alternative solution

d2 []        =  []
d2 (x : xs)  =  x : d2 (drop1 xs)
    where
      drop1 [] = []
      drop1 (x:xs) = xs

--2c.  

prop_cd :: String -> Bool
prop_cd xs  =  c xs == d xs
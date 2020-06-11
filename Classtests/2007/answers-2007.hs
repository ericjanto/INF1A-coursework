import Data.Char
  hiding (ord)
import Data.List
import Test.QuickCheck


-- Exercise 1

-- a

ordPairs :: [(Char,Int)]
ordPairs = zip ['A'..'Z'] [65..]

ordPairs' :: [(Char,Int)]
ordPairs' = zip ['a'..'z'] [97..]


ord :: Char -> Int
ord c | isUpper c = lookUp c ordPairs
      | isLower c = lookUp c ordPairs'
      | otherwise = error "invalid input"
      where
        lookUp c ps = head [v | (ch,v) <- ps, c == ch]

test_ord = ord 'A' == 65 && ord 'B' == 66 &&
           ord 'Z' == 90 && ord 'a' == 97 &&
           ord 'b' == 98 && ord 'z' == 122

f :: Char -> Int
f = sub . ord
  where
    sub n | n > 64 && n < 97 = n - 65
          | otherwise        = n - 97


test_f = f 'A' == 0  && f 'B' == 1 &&
         f 'Z' == 25 && f 'a' == 0 &&
         f 'b' == 1  && f 'z' == 25

-- b

g :: String -> Int
g xs = sum [f x | x <- xs, isAlpha x]

test_g = g "aBc4e" == 7 && g "?!" == 0

-- c

h :: String -> Int
h [] = 0
h (x:xs)
    | isAlpha x = f x + h xs
    | otherwise = h xs

alphs = ['a'..'z'] ++ ['A'..'Z']

prop_hg :: String -> Property
prop_hg s = and (map (`elem` alphs) s) ==> g s == h s

test_hg = quickCheck prop_hg

-- Exercise 2

-- a

c :: [Int] -> [Int] -> [Int]
c xs ys | length xs == length ys = [x - y | (x,y) <- zip xs ys]
        | otherwise              = error "not same length"

test_c = c [5,7,3] [1,2,4] == [4,5,-1]

-- b

d :: [Int] -> [Int] -> [Int]
d [] [] = []
d [] _  = error "not same length"
d _  [] = error "not same length"
d (x:xs) (y:ys) = (x-y) : d xs ys

prop_cd :: [Int] -> [Int] -> Property
prop_cd x y = length x == length y ==> c x y == d x y

test_cd = quickCheck (withMaxSuccess 10000 prop_cd) -- :(

-- c

e :: [Int] -> [Int] -> Bool
e xs ys = isNull $ c xs ys
    where
      isNull xs = and [x == 0 | x <- xs]

prop_e :: [Int] -> Bool
prop_e xs = e xs xs == True

test_e = quickCheck prop_e


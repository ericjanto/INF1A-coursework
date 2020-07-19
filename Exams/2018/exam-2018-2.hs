-- Informatics 1 Functional Programming
-- December 2018
-- SITTING 2 (14:30 - 16:30)

import Test.QuickCheck
import Control.Monad
import Data.Char

-- Question 1

-- 1a

f :: String -> Int
f s = sum [p | (d,p) <- zip s [0..], isDigit d]

test_f = f "" == 0 &&
         f "0 is the first position" == 0 &&
         f "I Love Functional Programming" == 0 &&
         f "2nite is 2 L8" == 21 &&
         f "0131 650 1000" == 66 &&
         f "1oTs & LoT5 of Num63r5" == 68
-- passed

-- 1b

g :: String -> Int
g s = f 0 s
    where
      f _ []     = 0
      f n (x:xs) | isDigit x = n + f (n+1) xs
                 | otherwise = f (n+1) xs


prop_fg :: String -> Bool
prop_fg s = f s == g s

test_fg = quickCheck prop_fg
-- passed

-- Question 2
-- rest is exactly like paper 1

-- 2a

p :: [(Int,Int)] -> Bool
p = undefined

-- 2b

q :: [(Int,Int)] -> Bool
q = undefined

-- 2c

r :: [(Int,Int)] -> Bool
r = undefined

-- Question 3

data Tree a = Lf a | Tree a :+: Tree a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized gen
    where
    gen 0 = liftM Lf arbitrary
    gen n | n>0 =
      oneof [liftM Lf arbitrary,
             liftM2 (:+:) tree tree]
      where
      tree = gen (n `div` 2)

-- 3a

left :: Tree a -> Bool
left = undefined

-- 3b

leaves :: Tree a -> [a]
leaves = undefined

-- 3c

shift :: Tree a -> Tree a
shift = undefined
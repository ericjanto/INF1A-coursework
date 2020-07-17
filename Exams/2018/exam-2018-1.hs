-- Informatics 1 Functional Programming
-- December 2018
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck
import Control.Monad
import Data.Char

-- Question 1

-- 1a

f :: String -> Int
f xs = sum [ y | (x,y) <- zip xs [0..], isUpper x]  

test_f = f "" == 0 && -- passed
         f "no capitals here" == 0 &&
         f "Positions start from 0" == 0 &&
         f "ALL CAPS" == 25 &&
         f "I Love Functional Programming" == 27 &&
         f "1oTs & LoT5 of Num63r5" == 33

-- 1b

g :: String -> Int
g xs = f 0 xs
    where
      f _ []     = 0
      f n (x:xs) | isUpper x = n + f (n+1) xs
                 | otherwise = f (n+1) xs

prop_fg :: String -> Bool
prop_fg s = f s == g s

test_fg = quickCheck prop_fg -- passed

-- Question 2

-- 2a

p :: [(Int,Int)] -> Bool
p ps = firsts > seconds
    where
      firsts  = sum [f*f | (f,_) <- ps]
      seconds = product [s | (_,s) <- ps, odd s] 

test_p = p []                  == False &&
         p [(4,5),(1,3)]       == True &&
         p [(4,5),(1,2),(2,7)] == False &&
         p [(-1,3),(1,1)]      == False &&
         p [(1,2),(2,3),(3,5)] == False &&
         p [(2,2),(2,3),(3,5)] == True

-- 2b

q :: [(Int,Int)] -> Bool
q ps = f 0 1 ps
    where
      f n m [] = n > m
      f n m ((a,b):ps)
          | odd b = f (n+a*a) (m*b) ps
          | otherwise = f (n+a*a) m ps


prop_pq :: [(Int,Int)] -> Bool
prop_pq ps = p ps == q ps

test_pq = quickCheck prop_pq -- passed

-- 2c

r :: [(Int,Int)] -> Bool
r ps = foldr (*) 1 odds < foldr (+) 0 evens
  where
    odds = map snd $ filter (\(_,y) -> odd y) ps
    snd (a,b) = b
    evens = map fst ps
    fst (a,b) = a*a

prop_pqr :: [(Int,Int)] -> Bool
prop_pqr ps = p ps == q ps && q ps == r ps

test_pqr = quickCheck prop_pqr
 
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

right :: Tree a -> Bool
right (Lf _) = True
right (Lf _ :+: xs) = right xs
right (_ :+: _) = False

test_right = right (Lf 1) == True && -- passed
             right (Lf 1 :+: (Lf 2 :+: (Lf 3 :+: (Lf 4 :+: Lf 5)))) == True &&
             right((Lf 1 :+: Lf 2) :+: (Lf 3 :+: Lf 4))  == False &&
             right (Lf "a" :+: (Lf "b" :+: Lf "c"))      == True &&
             right ((Lf "a" :+: Lf "b") :+: Lf "c")      == False

-- 3b

leaves :: Tree a -> [a]
leaves (Lf v) = [v]
leaves (xs :+: ys) = leaves xs ++ leaves ys 

test_leaves = leaves (Lf 1) == [1] && -- passed
              leaves (Lf 1 :+: (Lf 2 :+: (Lf 3 :+: (Lf 4 :+: Lf 5)))) == [1,2,3,4,5] &&
              leaves((Lf 1 :+: Lf 2) :+: (Lf 3 :+: Lf 4))  == [1,2,3,4] &&
              leaves (Lf "a" :+: (Lf "b" :+: Lf "c"))      == ["a","b","c"] &&
              leaves ((Lf "a" :+: Lf "b") :+: Lf "c")      == ["a","b","c"]

-- 3c

shift :: Tree a -> Tree a
shift (Lf x) = Lf x
shift tr | right tr  = tr
         | otherwise = genRight $ leaves tr
         where
            genRight [x] = Lf x 
            genRight (x:xs) = Lf x :+: genRight xs

prop_shift = shift (Lf 1)                                == (Lf 1)                                &&
             shift (Lf 1 :+: (Lf 2 :+: (Lf 3 :+: Lf 4))) == (Lf 1 :+: (Lf 2 :+: (Lf 3 :+: Lf 4))) &&
             shift ((Lf 1 :+: Lf 2) :+: (Lf 3 :+: Lf 4)) == (Lf 1:+: (Lf 2 :+: (Lf 3 :+: Lf 4)))  &&
             shift (Lf "a" :+: (Lf "b" :+: Lf "c"))      == (Lf "a" :+: (Lf "b" :+: Lf "c"))      &&
             shift ((Lf "a" :+: Lf "b") :+: Lf "c")      == (Lf "a" :+: (Lf "b" :+: Lf "c"))
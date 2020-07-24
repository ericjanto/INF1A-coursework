-- Informatics 1 Functional Programming
-- December 2016
-- SITTING 2 (14:30 - 16:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>), Property )
import Control.Monad -- defines liftM, liftM3, used below
import Data.List
import Data.Char

-- Question 1

-- 1a

f :: [Int] -> String -> Int
f xs s = product [x | (x,c) <- zip xs s, c == 'y']  

test_f =f [3,5,2,4,1] "yynyn"     == 60 &&
        f [10,20,30,40,50] "abcd" == 1 &&
        f [] "baby"               == 1 &&
        f [4,3,2,1] "aye"         == 3
-- passed :l

-- 1b

g :: [Int] -> String -> Int
g [] _ = 1
g _ [] = 1
g (x:xs) (y:ys) | y == 'y'  = x * g xs ys
                | otherwise = g xs ys

prop_fg :: [Int] -> String -> Bool
prop_fg xs s = f xs s == g xs s

test_fg = quickCheck prop_fg
-- passed :U

-- Question 2

-- 2a

p :: String -> Int
p xs = sum [digitToInt x | x <- xs, isDigit x, even $ digitToInt x]

test_p =  p "Functional"   == 0 &&
          p "42+12=54"     == 12 &&
          p "3.157/3 > 19" == 0 &&
          p "1234567890"   == 20
-- passed Ü

-- 2b

q :: String -> Int
q [] = 0
q (x:xs) | isDigit x = if even (digitToInt x) then digitToInt x + q xs else q xs
         | otherwise = q xs

prop_pq xs = p xs == q xs
test_pq = quickCheck prop_pq
-- passed hehehehhehehe

-- 2c

r :: String -> Int
r = foldr (+) 0 . filter even . map digitToInt . filter isDigit

prop_pqr xs = p xs == q xs && q xs == r xs
test_pqr = quickCheck prop_pqr
-- passed:*_*

-- Question 3
-- solutions in first sitting

data Move =
     Go Int            -- move the given distance in the current direction
   | Turn              -- reverse direction
   | Dance             -- dance in place, without changing direction
  deriving (Eq,Show)   -- defines obvious == and show

data Command =
     Nil                      -- do nothing
   | Command :#: Move         -- do a command followed by a move
  deriving Eq                 -- defines obvious ==

instance Show Command where   -- defines show :: Command -> String
  show Nil = "Nil"
  show (com :#: mov) = show com ++ " :#: " ++ show mov

type Position = Int
data Direction = L | R
  deriving (Eq,Show)          -- defines obvious == and show
type State = (Position, Direction)

-- For QuickCheck

instance Arbitrary Move where
  arbitrary = sized expr
    where
      expr n | n <= 0 = elements [Turn, Dance]
             | otherwise = liftM (Go) arbitrary

instance Arbitrary Command where
  arbitrary = sized expr
    where
      expr n | n <= 0 = oneof [elements [Nil]]
             | otherwise = oneof [ liftM2 (:#:) subform arbitrary
                                 ]
             where
               subform = expr (n-1)

instance Arbitrary Direction where
  arbitrary = elements [L,R]

-- 3a

state :: Move -> State -> State
state = undefined

-- 3b

trace :: Command -> State -> [State]
trace = undefined

-- 3c

dancify :: Command -> Command
dancify = undefined
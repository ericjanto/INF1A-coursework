-- Informatics 1 Functional Programming
-- December 2016
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>), Property )
import Control.Monad -- defines liftM, liftM3, used below
import Data.List
import Data.Char

-- Question 1

-- 1a

f :: [Int] -> [Int] -> Int
f xs ys = sum [x | (x,y) <- zip xs ys, mod x y == 0]

test_f = f [6,9,2,7] [2,3,5,1]       == 22 &&
         f [6,9,2] [2,3,5,1]         == 15 &&
         f [1,2,3,4,5] [5,4,3,2,1]   == 12 &&
         f [10,20,30,40] [3,4,5,6,7] == 50



-- 1b

g :: [Int] -> [Int] -> Int
g [] _ = 0
g _ [] = 0
g (x:xs) (y:ys) | mod x y == 0 = x + g xs ys
                | otherwise    = g xs ys


prop_fg xs ys = nonZero ys ==> f xs ys == g xs ys
    where
      nonZero ys = and [y /= 0 | y <- ys]
test_fg = quickCheck prop_fg
-- passed .P

-- Question 2

-- 2a

p :: String -> Int
p xs = maximum' [digitToInt x | x <- xs, isDigit x]
    where
      maximum' [] = 0
      maximum' a  = maximum a

test_p = p "Inf1-FP" == 1 &&
         p "Functional" == 0 &&
         p "1+1=2" == 2 &&
         p "3.157/3 > 19" == 9
-- passed /-)

-- 2b

q :: String -> Int
q s = f 0 s
    where
      f h [] = h
      f h (x:xs) | isDigit x && (digitToInt x) > h = f (digitToInt x) xs
                 | otherwise                       = f h xs

prop_pq xs = p xs == q xs
test_pq = quickCheck prop_pq
-- passed :B

-- 2c

r :: String -> Int
r = undefined

-- Question 3

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
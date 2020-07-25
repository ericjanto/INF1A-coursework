-- Informatics 1 Functional Programming
-- August 2017

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
import Control.Monad -- defines liftM, liftM3, used below
import Data.List
import Data.Char

-- Question 1

-- 1a

f :: String -> [Int] -> String
f s xs = concat [replicate x c | (x,c) <- zip xs s]

test_f =  f "abcde" [3,1,2,0,4] == "aaabcceeee" &&
          f "call" [3,-2,1,2,7] == "ccclll" &&
          f "raisin" [1,2,3,4]  == "raaiiissss" &&
          f "moose" [2]         == "mm" &&
          f "" [1,2,3]          == ""

-- 1b

g :: String -> [Int] -> String
g [] _ = ""
g _ [] = ""
g (c:s) (x:xs) = f x c ++ g s xs
    where
      f n c | n < 1     = ""
            | otherwise = c : f (n-1) c

prop_fg :: String -> [Int] -> Bool
prop_fg s xs = f s xs == g s xs

test_fg = quickCheck prop_fg

-- Question 2

-- 2a

p :: String -> Bool
p s = and [odd $ digitToInt d | d <- s, isDigit d]

test_p =  p "Inf1-FP" == True &&
          p "Functional" == True &&
          p "1+1=2" == False &&
          p "3.157/3 > 19" == True

-- 2b

q :: String -> Bool
q "" = True
q (s:ss) | isDigit s = if odd (digitToInt s) then q ss else False
         | otherwise = q ss

test_q =  q "Inf1-FP" == True &&
          q "Functional" == True &&
          q "1+1=2" == False &&
          q "3.157/3 > 19" == True

-- 2c

r :: String -> Bool
r = foldr (&&) True . map odd . map digitToInt . filter isDigit

prop_pqr s = p s == q s && q s == r s
test_pqr = quickCheck prop_pqr

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

-- most of the solutions are in sitting 1
-- 3a

state :: Move -> State -> State
state = undefined

-- 3b

finalstate :: Command -> State -> State
finalstate = undefined

-- 3c

simplify :: Command -> Command
simplify = undefined
-- Informatics 1 Functional Programming
-- August 2016

import Test.QuickCheck
import Control.Monad -- defines liftM, liftM3, used below
import Data.List
import Data.Char

-- Question 1

-- 1a

f :: String -> Int
f xs = sum [digitToInt x * 2^y | (x,y) <- zip (reverse (xs)) [0..]]

test_f =  f "101" == 5 &&
          f "11" == 3 &&
          f "1101" == 13 &&
          f "110111" == 55

-- 1b

g :: String -> Int
g xs = f (reverse xs) 0
  where
    f [] _ = 0
    f (x:xs) y = (digitToInt x) * 2^y + (f xs (y+1))

test_g = g "101" == 5 &&
         g "11" == 3 &&
         g "1101" == 13 &&
         g "110111" == 55

prop_fg :: String -> Property
prop_fg s = and (map isDigit s) ==> f s == g s

test_fg = quickCheck prop_fg

-- Question 2

-- 2a

p :: [Int] -> Bool
p xs = and [odd x | x <- xs, mod x 3 == 0]

test_p = p [1,15,153,83,64,9] == True &&
  p [1,12,153,83,9] == False &&
  p [] == True &&
  p [2,151] == True
-- passed

-- 2b

q :: [Int] -> Bool
q [] = True
q (x:xs) | mod x 3 == 0 = odd x && q xs
         | otherwise    = q xs

test_q = q [1,15,153,83,64,9] == True &&
  q [1,12,153,83,9] == False &&
  q [] == True &&
  q [2,151] == True
-- passed

-- 2c

r :: [Int] -> Bool
r = foldr (&&) True . map odd . filter (\x -> mod x 3 == 0)

prop_pqr :: [Int] -> Bool
prop_pqr xs = p xs == q xs &&
             q xs == r xs

test_pqr = quickCheck prop_pqr
-- passed

-- Question 3

data Prop = X
          | F
          | T
          | Not Prop
          | Prop :->: Prop
          deriving (Eq, Ord)

-- turns a Prop into a string approximating mathematical notation

showProp :: Prop -> String
showProp X           =  "X"
showProp F           =  "F"
showProp T           =  "T"
showProp (Not p)     =  "(~" ++ showProp p ++ ")"
showProp (p :->: q)  =  "(" ++ showProp p ++ "->" ++ showProp q ++ ")"

-- For QuickCheck

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:->:) subform subform
                                       ]
                 where
                   atom = oneof [elements [X,F,T]]
                   subform  =  prop (n `div` 2)

-- 3a

eval :: Prop -> Bool -> Bool
eval F _ = False
eval T _ = True
eval (Not X) True = False
eval (Not X) False = True
eval (Not x) b = not $ (eval x) b
eval (a :->: b) x  = (eval a x) `imply` (eval b x)
  where
    imply :: Bool -> Bool -> Bool
    imply True False = False
    imply _ _        = True

test_eval = eval (Not T) True                    ==  False &&
            eval (Not X) False                   ==  True &&
            eval (Not X :->: Not (Not X)) True   ==  True &&
            eval (Not X :->: Not (Not X)) False  ==  False &&
            eval (Not (Not X :->: F)) True       ==  False &&
            eval (Not (Not X :->: F)) False      ==  True
-- passed^^

-- 3 b

simplify :: Prop -> Prop
simplify = undefined
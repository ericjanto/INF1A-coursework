-- Informatics 1 Functional Programming
-- August 2018

module Aug2018 where

import Test.QuickCheck
import Control.Monad -- defines liftM, liftM2, liftM3, used below
import Data.Char

-- Question 1

f :: [String] -> [String]
f xs = [last y : tail x | (x,y) <- zip xs $ tail xs] 

test_f =  f ["pattern","matching","rules","ok"] == ["gattern","satching","kules"] &&
          f ["word"]   == [] &&
          f ["almost","all","students","love","functional","programming"] == ["llmost","sll","etudents","love","gunctional"] &&
          f ["make","love","not","war"] == ["eake","tove","rot"]

g :: [String] -> [String]
g []  = []
g [x] = []
g (x:y:xs) = (last y : tail x) : g (y:xs)

prop_fg :: [String] -> Property
prop_fg xs = nonEmpty xs ==> f xs == g xs
    where
      nonEmpty :: [String] -> Bool
      nonEmpty xs = and [not (null x) | x <- xs]
test_fg    = quickCheck prop_fg
-- passed hehe

-- Question 2

-- 2a

p :: [String] -> Int
p = undefined

-- 2b

q :: [String] -> Int
q = undefined

-- 2c

r :: [String] -> Int
r = undefined

-- Question 3

data Expr = X                      -- variable X
          | Y                      -- variable Y
          | Const Int              -- integer constant
          | Expr :+: Expr          -- addition
          | Expr :*: Expr          -- multiplication
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr Y          =  "Y"
showExpr (Const n)  =  show n
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [ return X
                                       , return Y
                                       , liftM Const arbitrary ]
                 | otherwise  =  oneof [ return X
                                       , return Y
                                       , liftM Const arbitrary
                                       , liftM2 (:+:) subform2 subform2
                                       , liftM2 (:*:) subform2 subform2
                                       ]
                 where
                   subform2  =  expr (n `div` 2)

-- 3a

eval :: Expr -> Int -> Int -> Int
eval = undefined

-- 3b

isSimple :: Expr -> Bool
isSimple = undefined

-- 3c

simplify :: Expr -> Expr
simplify = undefined
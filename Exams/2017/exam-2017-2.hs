-- Informatics 1 Functional Programming
-- December 2017
-- SITTING 2 (14:30 - 16:30)

module Dec2017 where

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ), Gen, suchThat,
                        oneof, elements, sized, (==>) )
import Control.Monad -- defines liftM, liftM2, liftM3, used below
import Data.Char

-- Question 1

f :: [Int] -> [String]
f xs = [if x < y then "<" else ">" | (x,y) <- zip xs $ tail xs, x /= y]

test_f =  f [4,2,5,6,1,8] == [">","<","<",">","<"] &&
          f [] == [] &&
          f [3] == [] &&
          f [3,3,1,-3] == [">",">"]
-- passed :v)
 
g :: [Int] -> [String]
g [] = []
g [x] = []
g (x:y:xs) | x < y     = "<" : g (y:xs)
           | x > y     = ">" : g (y:xs)
           | otherwise = g (y:xs)

prop_fg xs = f xs == g xs
test_fg    = quickCheck prop_fg
-- passed B)

-- Question 2 -- remaining sol are in first sitting.

-- 2a

isInitialism :: String -> Bool
isInitialism = undefined

p :: [String] -> Int
p = undefined

-- 2b

isInitialism' :: String -> Bool
isInitialism' = undefined

q :: [String] -> Int
q = undefined

-- 2c

r :: [String] -> Int
r = undefined

-- Question 3

data Expr = X                      -- variable
          | Const Int              -- integer constant >=0
          | Expr :+: Expr          -- addition
          | Expr :*: Expr          -- multiplication
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
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
                                       , liftM Const genPos ]
                 | otherwise  =  oneof [ return X
                                       , liftM Const genPos
                                       , liftM2 (:+:) subform2 subform2
                                       , liftM2 (:*:) subform2 subform2
                                       ]
                 where
                   subform2  =  expr (n `div` 2)
                   genPos  =  oneof [ return 0, return 1, return 2, return 3, return 4,
                                      return 5, return 6, return 7, return 8, return 8 ]

-- 3a

eval :: Expr -> Int -> Int
eval = undefined
-- 3b

isSimple :: Expr -> Bool
isSimple = undefined

-- 3c

simplify :: Expr -> Expr
simplify = undefined
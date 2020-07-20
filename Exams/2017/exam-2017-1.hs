-- Informatics 1 Functional Programming
-- December 2017
-- SITTING 1 (09:30 - 11:30)

module Dec2017 where

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ), Gen, suchThat,
                        oneof, elements, sized, (==>) )
import Control.Monad -- defines liftM, liftM2, liftM3, used below
import Data.Char

-- Question 1

f :: [Int] -> [Int]
f xs = [ y - x | (x,y) <- zip xs (tail xs), x < y ]

test_f =  f [4,2,5,6,1,8] == [3,1,7] &&
          f [] == [] &&
          f [3] == [] &&
          f [3,3,1,-3] == []
  
g :: [Int] -> [Int]
g []  = []
g [x] = []
g (x:y:xs) | x < y     = (y - x) : g (y:xs)
           | otherwise = g (y:xs)

prop_fg xs = f xs == g xs
test_fg    = quickCheck prop_fg
-- passed :D
  
-- Question 2

-- 2a

isInitialism :: String -> Bool
isInitialism s | length s < 2 = False
               | otherwise    = and [isUpper c | c <- s]

p :: [String] -> Int
p ss = length $ concat [s | s <- ss, isInitialism s]

test_isInitialism = isInitialism "A" == False &&
                    isInitialism "AWOL" == True &&
                    isInitialism "Ltd" == False
-- passed d8)

test_p =  p ["I","played","the","BBC","DVD","on","my","TV"] == 8 &&
          p ["The","DUP","MP","is","not","OK"] == 7 &&
          p ["The","SNP","won","in","South","Morningside"] == 3 &&
          p [] == 0
-- passed ;)

-- 2b

isInitialism' :: String -> Bool
isInitialism' s = f 0 s
    where
      f 0 s | length s < 2 = False
      f _ []               = True
      f n (c:s)            = isUpper c && f (n+1) s


test_isInitialism' = isInitialism' "A" == False &&
                     isInitialism' "AWOL" == True &&
                     isInitialism' "Ltd" == False
-- passed :'D

q :: [String] -> Int
q [] = 0
q (s:ss) | isInitialism' s = length s + q ss
         | otherwise       = q ss

test_q =  q ["I","played","the","BBC","DVD","on","my","TV"] == 8 &&
          q ["The","DUP","MP","is","not","OK"] == 7 &&
          q ["The","SNP","won","in","South","Morningside"] == 3 &&
          q [] == 0
-- passed :-()

-- 2c

r :: [String] -> Int
r = foldr (+) 0 . map length . filter isInitialism

prop_pqr ss = p ss == q ss && q ss == r ss
test_pqr    = quickCheck prop_pqr
-- passed $)

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
eval (X) v       = v
eval (Const a) v = a
eval (a :*: b) v = eval a v * eval b v
eval (a :+: b) v = eval a v + eval b v

test_eval = eval ((X :*: Const 3) :+: (Const 0 :*: X)) 2 == 6 &&
            eval (X :*: (Const 3 :+: Const 4)) 2         == 14 &&
            eval (Const 4 :+: (Const 3 :*: X)) 3         == 13 &&
            eval (((Const 1 :*: Const 2) :*: (X :+: Const 1)) :*: Const 2) 3 == 16
-- passed o.O            

-- 3b

isSimple :: Expr -> Bool
isSimple (Const _ :*: _) = False
isSimple (a :*: b) = isSimple a && isSimple b
isSimple (a :+: b) = isSimple a && isSimple b
isSimple _         = True

test_isSimple = isSimple ((X :*: Const 3) :+: (Const 0 :*: X)) == False &&
                isSimple (X :*: (Const 3 :+: Const 4))         == True &&
                isSimple (Const 4 :+: (Const 3 :*: X))         == False &&
                isSimple (((Const 1 :*: Const 2) :*: (X :+: Const 1)) :*: Const 2) == False
-- passed !!!!!!!

-- 3c

simplify :: Expr -> Expr
simplify (Const 0 :*: e) = Const 0
simplify (Const 1 :*: e) = e
simplify (Const n :*: e) = f n e
    where
      f 1 e = simplify e
      f n e = e :*: f (n-1) (simplify e)
simplify (a :*: b) = simplify a :*: simplify b
simplify (a :+: b) = simplify a :+: simplify b
simplify remain = remain

test_simplify = simplify ((X :*: Const 3) :+: (Const 0 :*: X)) == (X :*: Const 3) :+: Const 0 &&
                simplify (X :*: (Const 3 :+: Const 4)) == X :*: (Const 3 :+: Const 4) &&
                simplify (((Const 1 :*: Const 2) :*: (X :+: Const 1)) :*: Const 2) == ((X :+: Const 1) :+: (X :+: Const 1)) :*: Const 2 &&
                (simplify (Const 4 :+: (Const 3 :*: X)) == Const 4 :+: (X :+: (X :+: X)) ||
                simplify (Const 4 :+: (Const 3 :*: X)) == Const 4 :+: ((X :+: X) :+: X)
                )
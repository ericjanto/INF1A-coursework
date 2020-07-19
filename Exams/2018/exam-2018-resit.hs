import Test.QuickCheck
import Data.Char
import Control.Monad


-- Question 1

-- 1a

f :: String -> Bool
f s = and [even pos | (lc, pos) <- zip s [0..], isLower lc]

test_f = f "" == True &&
         f "ALL CAPS" == True &&
         f "I LOvE FuNcTiOnAL pRoGrAmMiNg" == True &&
         f "aLterNaTiNg" == False &&
         f "aLtErNaTiNg" == True &&
         f "WEe" == True
-- passed


-- 1b

g :: String -> Bool
g s = f 0 s
    where
      f _ []     = True
      f n (x:xs) | isLower x = even n && f (n+1) xs
                 | otherwise = f (n+1) xs

prop_fg :: String -> Bool
prop_fg s = f s == g s

test_fg = quickCheck prop_fg
-- passed

-- Question 2

-- 2a

p :: [(Int,Bool)] -> Bool
p xs = even $ sum [v*v | (v,b) <- xs, b]

test_p =  p [] == True &&
          p [(3,False)] == True &&
          p [(7,True)] == False &&
          p [(3,True),(2,True),(5,True)] == True &&
          p [(3,False),(2,True),(5,True)] == False &&
          p [(4,False),(3,True)] == False
                  
-- 2b

q :: [(Int,Bool)] -> Bool
q ps = even $ sum ps
    where
      sum [] = 0
      sum ((v,b):ps) | b         = v*v + sum ps
                     | otherwise = sum ps

prop_pq :: [(Int,Bool)] -> Bool
prop_pq ps = p ps == q ps

test_pq = quickCheck prop_pq
-- passed

-- 2c

r :: [(Int,Bool)] -> Bool
r = even . foldr (+) 0 . map rmv . filter (\(v,b) -> b)
  where
    rmv (v,b) = v

prop_pr :: [(Int,Bool)] -> Bool
prop_pr ps = p ps == r ps

test_pr = quickCheck prop_pr
-- passed

-- Question 3

type Nat = Int
data Term = Tm Nat Nat  deriving (Eq, Show)
data Poly = Pl [Term]   deriving (Eq, Show)
data Expr
  = X
  | C Nat
  | Expr :+: Expr
  | Expr :*: Expr
  | Expr :^: Expr
  deriving (Eq, Show)

nat :: Gen Int 
nat =  liftM abs arbitrary

instance Arbitrary Term where
  arbitrary = liftM2 Tm nat nat

instance Arbitrary Poly where
  arbitrary = liftM Pl arbitrary

showExpr :: Expr -> String
showExpr =  show

showTerm :: Term -> String
showTerm =  show

showPoly :: Poly -> String
showPoly =  show

expr0 :: Expr
expr0 = ((C 1 :*: (X :^: C 0)) :+:
        ((C 2 :*: (X :^: C 1)) :+:
        ((C 3 :*: (X :^: C 2)) :+:
        C 0)))

poly0 :: Poly
poly0 =  Pl [Tm 1 0, Tm 2 1, Tm 3 2]

-- 3a

evalExpr :: Expr -> Int -> Int
evalExpr (C a) v = a
evalExpr (X) v = v
evalExpr (a :*: b) v = (evalExpr a v) * (evalExpr b v)
evalExpr (a :+: b) v = (evalExpr a v) + (evalExpr b v)
evalExpr (a :^: b) v = (evalExpr a v) ^ (evalExpr b v)


test_evalExpr = evalExpr ((C 1 :*: (X :^: C 0))) 5 == 1  &&
                evalExpr ((C 2 :*: (X :^: C 1))) 5 == 10 &&
                evalExpr ((C 3 :*: (X :^: C 2))) 5 == 75 &&
                evalExpr (C 0) 5                   == 0  &&
                evalExpr expr0 5                   == 86 &&
                evalExpr expr0 10                  == 321
-- passed :O
       
-- 3b

evalTerm :: Term -> Int -> Int
evalTerm (Tm c p) v = c * (v^p)  

test_evalTerm = evalTerm (Tm 1 0) 5  ==  1  &&
                evalTerm (Tm 2 1) 5  ==  10 && 
                evalTerm (Tm 3 2) 5  ==  75
-- passed

evalPoly :: Poly -> Int -> Int
evalPoly (Pl tms) v = sum $ map ((flip evalTerm) v) tms 

test_evalPoly = evalPoly (Pl []) 5 == 0   &&
                evalPoly poly0 5   == 86  &&
                evalPoly poly0 10  == 321
-- passed :)

-- 3c

exprTerm :: Term -> Expr
exprTerm (Tm c p) = C c :*: (X :^: C p)

test_exprTerm = exprTerm (Tm 1 0) == C 1 :*: (X :^: C 0) &&
                exprTerm (Tm 2 1) == C 2 :*: (X :^: C 1) &&
                exprTerm (Tm 3 2) == C 3 :*: (X :^: C 2)
-- passed ^^

exprPoly :: Poly -> Expr
exprPoly (Pl [])  = C 0
exprPoly (Pl [x]) = exprTerm x :+: C 0
exprPoly (Pl (x:xs)) = exprTerm x :+: exprPoly (Pl xs)

-- exprPoly (Pl tms) = foldr (:+:) (C 0) (map exprTerm tms) <<- is much nicer

test_exprPoly = exprPoly (Pl []) == C 0 &&
                exprPoly poly0   == expr0
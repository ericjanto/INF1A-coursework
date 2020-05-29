module Answers where


import Test.QuickCheck



-- 1a)
f :: [Int] -> Int
f xs = sum [x^2 | x <- xs, x `mod` 3 == 0, x `mod` 5 /= 0]

test1a =
  f []            == 0  &&
  f [9,-3]        == 90 &&
  f [0,30,2,7]    == 0  &&
  f [-6,15,2,1,3] == 45

-- 1b)
g :: [Int] -> Int
g [] = 0
g (x:xs)
  | p x       = x^2 + g xs
  | otherwise = g xs
  where
    p x = mod x 3 == 0 && mod x 5 /= 0

test1b =
  g []            == 0  &&
  g [9,-3]        == 90 &&
  g [0,30,2,7]    == 0  &&
  g [-6,15,2,1,3] == 45


-- 1c)
prop_fg :: [Int] -> Bool
prop_fg xs = g xs == f xs

test1c = quickCheck prop_fg

-- 2a)
mst :: Int -> Int -> Bool
mst x y
    | x >= 0 = x*2 < y
    | x < 0  = div x 2 < y

test2a =
  mst (-10) (-5) == False &&
  mst 7 14 == False &&
  mst (-10) (-4) == True &&
  mst 7 15 == True &&
  mst (-2) 3 == True

-- 2b)
ordered :: [Int] -> Bool
ordered xs = and [mst x y | (x,y) <- xs `zip` tail xs]

test2b =
  ordered [] == True &&
  ordered [-4,-1,3,1,9] == False &&
  ordered [-4,-1,1,3,9] == True &&
  ordered [-4,-1,1,2,9] == False

-- 2c)
ordered' :: [Int] -> Bool
ordered' []  = True
ordered' [x] = True
ordered'(x:y:xs)
    | mst x y   = True && ordered' (y:xs)
    | otherwise = False


prop_ordered :: [Int] -> Bool
prop_ordered xs = ordered xs == ordered' xs

test2c = quickCheck prop_ordered
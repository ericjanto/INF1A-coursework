import Test.QuickCheck

-- Exercise 1
-- a

sign :: Int -> Char
sign x | x >=  1 && x <=  9 = '+'
       | x ==  0             = '0'
       | x >= -9 && x <= -1 = '-'
       | otherwise          = error "invalid input"

-- b

signs :: [Int] -> String
signs xs = [sign x | x <- xs, x >= -9 && x <= 9 ]

test_signs = signs [5,10,-5,0] == "+-0"

-- c

signsRec :: [Int] -> String
signsRec [] = []
signsRec (x:xs)
    | x >= -9 && x <= 9 = sign x : signsRec xs
    | otherwise         = signsRec xs

prop_signs :: [Int] -> Bool
prop_signs xs = signs xs == signsRec xs

test_signs' = quickCheck prop_signs

-- Exercise 2
-- a

upcount :: [Int] -> Int
upcount xs = sum [if x < y then 1 else 0 | (x,y) <- zip xs $ tail xs]

test_upcount = upcount [1,7,2,2,5,6] == 3

-- b

upcountRec :: [Int] -> Int
upcountRec [] = 0
upcountRec [x] = 0
upcountRec (x:y:xs)
    | x < y     = 1 + upcountRec (y:xs)
    | otherwise = upcountRec (y:xs)


prop_upcount :: [Int] -> Bool
prop_upcount xs = upcount xs == upcountRec xs

test_upcount' = quickCheck prop_upcount

-- c

ascending :: [Int] -> Bool
ascending xs = length xs - 1 == upcount xs

test_ascending = ascending [-9,0,1,2,3,4] == True && ascending [0,1,2,0] == False
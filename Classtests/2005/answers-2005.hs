import Test.QuickCheck hiding (total)

-- Exercise 1

type Pounds = Float
type Percent = Float
type Quantity = Int

-- a

discount :: Quantity -> Percent
discount q
    | q >= 1  && q <= 9  = 0.0
    | q >= 10 && q <= 40 = 0.1
    | q >= 50            = 0.2
    | otherwise          = error "invalid input"

test_discount = discount 5   == 0.0 && discount 20 == 0.1 &&
                discount 100 == 0.2

-- b

total :: [(Quantity,Pounds)] -> Pounds
total ps = sum [p * fromIntegral q * (1 - discount q) | (q,p) <- ps, q > 0]

test_total = total [(5, 4.00), (-1, 1000.00), (20, 10.00), (100, 2.00)] == 360.00

-- c

totalRec :: [(Quantity,Pounds)] -> Pounds
totalRec [] = 0
totalRec ((q,p):ps)
    | q > 0     = p * fromIntegral q * (1 - discount q) + totalRec ps
    | otherwise = totalRec ps


-- Exercise 2

-- Sample solutions for inf1a programming class test 2005.

type Pounds = Float
type Percent = Float
type Quantity = Int

-- 1 (a)
discount :: Quantity -> Percent
discount q
    | q >= 50 = 0.2
    | q >= 10 = 0.1
    | q >= 1  = 0.0
    | otherwise = error "Discount of negative or zero quantity!"

-- 1 (b)
total :: [(Quantity, Pounds)] -> Pounds
total items = sum [p * fromIntegral q * (1 - discount q) | (q,p) <- items, q > 0]

-- 1 (c)
total' :: [(Quantity, Pounds)] -> Pounds
total' [] = 0.0
total' ((q,p):xs)
    | q <= 0    = total xs
    | otherwise = p * fromIntegral q * (1 - discount q) + total xs


-- 2 (a)
changes :: [Int] -> [Int]
changes xs = [x | (x,y) <- zip xs (tail xs), x /= y]


-- 2 (b)
changes' :: [Int] -> [Int]
changes' [] = []
changes' [_] = []
changes' (x:y:xs) | x == y    = changes (y:xs)
                  | otherwise = x : changes (y:xs)

-- 2 (c)
alleq :: [Int] -> Bool
alleq xs = changes xs == []
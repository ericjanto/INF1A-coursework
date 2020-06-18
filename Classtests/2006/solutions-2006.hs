--1a.

sign x | 0 < x && x <= 9     = '+'
       | 0 == x              = '0'
       | 0 > x && x >= (-9)  = '-'
       | otherwise           = error ("sign: argument "++show x++" is out of range")

--1b.

signs xs = [sign x | x <- xs, (-9) <= x, x <= 9]

--1c.

signs' [] = ""
signs' (x:xs) | x <= 9 && x >= (-9) = sign x : signs' xs
              | otherwise           = signs' xs

--2a.

upcount xs = sum [1 | (x, x') <- zip xs (tail xs), x < x']

--2b.

upcount' [] = 0
upcount' [x] = 0
upcount' (x:y:xs) | x < y     = 1 + upcount' (y:xs)
                  | otherwise = upcount' (y:xs)

--2c.

ascending xs = upcount xs == length xs -1

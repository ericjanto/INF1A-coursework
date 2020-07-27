squaresCond :: [Integer] -> [Integer]
squaresCond someList = 
    if null someList then
      []
    else
      let
        x  = head someList
        xs = tail someList
      in 
        x * x : squaresCond xs


sumSqOdds :: [Int] -> Int
sumSqOdds [] = 0
sumSqOdds (x:xs)
    | odd x = x * x + sumSqOdds xs
    | otherwise = sumSqOdds xs
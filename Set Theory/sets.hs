import Data.List


-- |{n ≤ 100 : n ∈ N ∧ ∃m ∈ N (n = 2m + 1)}|
set1 :: Int
set1 = length [x | x <- [0..100], odd x]
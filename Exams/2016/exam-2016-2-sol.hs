import Test.QuickCheck( quickCheck,
Arbitrary( arbitrary ),
oneof, elements, sized, (==>), Property )
import Control.Monad -- defines liftM, liftM3, used below
import Data.List
import Data.Char
-- Question 1
-- 1a
f :: [Int] -> String -> Int
f ns cs = product [ n | (n,c) <- zip ns cs, c == ’y’ ]
test1a =
f [3,5,2,4,1] "yynyn" == 60 &&
f [10,20,30,40,50] "abcd" == 1 &&
f [] "baby" == 1 &&
f [4,3,2,1] "aye" == 3
-- 1b
g :: [Int] -> String -> Int
g [] _ = 1
g _ "" = 1
g (n:ns) (’y’:cs) = n * g ns cs
g (n:ns) (_:cs) = g ns cs
test1b =
g [3,5,2,4,1] "yynyn" == 60 &&
g [10,20,30,40,50] "abcd" == 1 &&
g [] "baby" == 1 &&
g [4,3,2,1] "aye" == 3
prop1 :: [Int] -> String -> Bool
prop1 ns cs = f ns cs == g ns cs
-- Question 2
i
-- 2a
p :: String -> Int
p cs = sum [ if even (digitToInt c) then digitToInt c else 0
| c <- cs, isDigit c ]
test2a =
p "Functional" == 0 &&
p "3.157/3 > 19" == 0 &&
p "42+12=54" == 12 &&
p "1234567890" == 20
-- 2b
q :: String -> Int
q [] = 0
q (c:cs) | isDigit c && even (digitToInt c) = digitToInt c + q cs
| otherwise = q cs
test2b =
q "Functional" == 0 &&
q "3.157/3 > 19" == 0 &&
q "42+12=54" == 12 &&
q "1234567890" == 20
-- 2c
r :: String -> Int
r cs = foldr (+) 0 (filter even (map digitToInt (filter isDigit cs)))
test2c =
r "Functional" == 0 &&
r "3.157/3 > 19" == 0 &&
r "42+12=54" == 12 &&
r "1234567890" == 20
prop2 :: String -> Bool
prop2 cs = p cs == q cs && q cs == r cs
-- Question 3
data Move =
Go Int -- move the given distance in the current direction
| Turn -- reverse direction
| Dance -- dance in place, without changing direction
deriving (Eq,Show) -- defines obvious == and show
data Command =
ii
Nil -- do nothing
| Command :#: Move -- do a command followed by a move
deriving Eq -- defines obvious ==
instance Show Command where -- defines show :: Command -> String
show Nil = "Nil"
show (com :#: mov) = show com ++ " :#: " ++ show mov
type Position = Int
data Direction = L | R
deriving (Eq,Show) -- defines obvious == and show
type State = (Position, Direction)
-- For QuickCheck
instance Arbitrary Move where
arbitrary = sized expr
where
expr n | n <= 0 = elements [Turn, Dance]
| otherwise = liftM (Go) arbitrary
instance Arbitrary Command where
arbitrary = sized expr
where
expr n | n <= 0 = oneof [elements [Nil]]
| otherwise = oneof [ liftM2 (:#:) subform arbitrary
]
where
subform = expr (n-1)
instance Arbitrary Direction where
arbitrary = elements [L,R]
-- 3a
state :: Move -> State -> State
state (Go d) (n,L) = (n - d, L)
state (Go d) (n,R) = (n + d, R)
state Turn (c,L) = (c, R)
state Turn (c,R) = (c, L)
state Dance p = p
test3a =
state (Go 3) (0,R) == (3,R) &&
state (Go 3) (0,L) == (-3,L) &&
state Turn (-2,L) == (-2,R) &&
state Dance (4,R) == (4,R)
iii
-- 3b
trace :: Command -> State -> [State]
trace Nil s = [s]
trace (com :#: mov) s = t ++ [state mov (last t)]
where t = trace com s
test3b =
trace (Nil) (3,R)
== [(3,R)] &&
trace (Nil :#: Go 3 :#: Turn :#: Go 4) (0,L)
== [(0,L),(-3,L),(-3,R),(1,R)] &&
trace (Nil :#: Go 3 :#: Turn :#: Dance :#: Turn) (0,R)
== [(0,R),(3,R),(3,L),(3,L),(3,R)] &&
trace (Nil :#: Go 3 :#: Turn :#: Go 2 :#: Go 1 :#: Turn :#: Go 4) (4,L)
== [(4,L),(1,L),(1,R),(3,R),(4,R),(4,L),(0,L)]
-- 3c
furtherpos :: State -> [State] -> Bool
furtherpos (p,s) ss = and [ abs p > abs q | (q,_) <- ss ]
dancify :: Command -> Command
dancify Nil = Nil
dancify (com :#: Dance) = (dancify com) :#: Dance
dancify (com :#: m) | furtherpos (state m (last t)) t
= (dancify com) :#: m :#: Dance
| otherwise = (dancify com) :#: m
where t = trace com (0,R)
test3c =
dancify Nil
== Nil &&
dancify (Nil :#: Go 3 :#: Turn :#: Go 4)
== Nil :#: Go 3 :#: Dance :#: Turn :#: Go 4 &&
dancify (Nil :#: Go 3 :#: Turn :#: Dance :#: Turn)
== Nil :#: Go 3 :#: Dance :#: Turn :#: Dance :#: Turn &&
dancify (Nil :#: Go 3 :#: Turn :#: Go 2 :#: Go 1 :#: Turn :#: Go 4)
== Nil :#: Go 3 :#: Dance :#: Turn :#: Go 2 :#: Go 1
:#: Turn :#: Go 4 :#: Dance
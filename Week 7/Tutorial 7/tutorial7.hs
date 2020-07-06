-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 7
--
-- Week 7(29 Oct.- 01 Nov.)

-- module Main where

-- ghc --make tutorial7.hs -package random -package QuickCheck -package OpenGL -package GLUT
-- ./tutorial7

-- ghci
-- :set -package random
-- :set -package OpenGL
-- :set -package GLUT
-- :l tutorial7

import LSystem
import Test.QuickCheck

pathExample = (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 302)

-- 1a. split
split :: Command -> [Command]
split (lh :#: rh)
  | isCommand lh && notSit lh = lh : split rh
  | not (isCommand lh)        = split lh ++ split rh
  | not (notSit lh)           = split rh
  where
    isComamnd (Go _)   = True
    isCommand (Turn _) = True
    isCommand (Sit)    = True
    isCommand _        = False
    notSit (Sit)       = False
    notSit _           = True
split (cmd) = [cmd]

test_split = split (Go 3 :#: Turn 4 :#: Go 7) == [Go 3, Turn 4, Go 7]

-- 1b. join
join :: [Command] -> Command
join [cmd] = cmd
join (cmd:cmds) = cmd :#: join cmds

test_join = join [Go 3, Turn 4, Go 7] == Go 3 :#: (Turn 4 :#: Go 7 :#: Sit)

-- 1c. equivalent
-- equivalent 
equivalent :: Command -> Command -> Bool
equivalent c1 c2 = split c1 == split c2

test_equivalent = ((Go 3 :#: Turn 4) :#: (Sit :#: Go 7)) `equivalent` (((Sit :#: Go 3) :#: Turn 4) :#: Go 7)

-- 1d. testing join and split
-- prop_split_join 
prop_split_join :: Command -> Bool
prop_split_join c = join (split c) `equivalent` c

-- prop_split
prop_split :: Command -> Bool
prop_split c = onlyValids $ split c
  where
    onlyValids ls = and [matches c | c <- ls]
    matches (Go _)   = True
    matches (Turn _) = True
    matches _ = False


-- 2a. copy
copy :: Int -> Command -> Command
copy x c = join $ take y (cycle (split c))
  where
    y = x * length (split c)

test_copy = copy 3 (Go 10 :#: Turn 120) ==
                   Go 10.0 :#: (Turn 120.0 :#: (Go 10.0 :#: (Turn 120.0 :#: (Go 10.0 :#: Turn 120.0))))

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d e = copy e (Go d :#: Turn angle)
  where
    angle = 360 / fromIntegral e  

prop_polygon :: Distance -> Bool
prop_polygon d = pentagon d `equivalent` polygon d 5

-- 3. spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral side n step angle
    | n == 0 || side <= 0 = Sit
    | otherwise           = Go side :#: Turn angle :#: (spiral (side + step) (n-1) step angle)


-- 4. optimise
-- Remember that Go does not take negative arguments.

optimise :: Command -> Command
optimise c = join $ filter clean (adj (filter clean (split c)))
  where
    clean (Go 0)   = False
    clean (Turn 0) = False
    clean _        = True
    adj [x]        = [x]
    adj (Go a   : Go b   : xs)     = adj (Go (a + b) : xs)
    adj (Turn a : Turn b : xs) = adj (Turn (a + b) : xs)
    adj (x:xs) = x : adj xs

test_optimise = optimise (Go 10 :#: Sit :#: Go 20 :#:Turn 35 :#: Go 0 :#: Turn 15 :#: Turn (-50)) `equivalent` Go 30.0

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
    where
      f 0 = GrabPen red :#: Go 10
      f x = g (x-1) :#: n :#: f (x-1) :#: n :#: g (x-1)
      g 0 = GrabPen blue :#: Go 10
      g x = f (x-1) :#: p :#: g (x-1) :#: p :#: f (x-1)
      n   = Turn 60
      p   = Turn (-60)


-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x
    where
      f 0 = GrabPen red :#: Go 10 :#: n :#: n :#: GrabPen blue :#: Go 10 :#: n :#: n :#: GrabPen green :#: Go 10 :#: n :#: n
      f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1) 
      n   = Turn 60
      p   = Turn (-60)


-- 7. hilbert
hilbert :: Int -> Command
hilbert x = l x
  where
    l 0 = GrabPen black :#: Go 10
    l x = p :#: r (x-1) :#: f :#: n :#: l (x-1) :#: f :#: l (x-1) :#: n :#: f :#: r (x-1) :#: p
    r 0 = GrabPen red :#: Go 10
    r x = n :#: l (x-1) :#: f :#: p :#: r (x-1) :#: f :#: r (x-1) :#: p :#: f :#: l (x-1) :#: n
    f   = Turn 45
    n   = Turn 90
    p   = Turn (-90)

--------------------------------------------------
--------------------------------------------------
---------------- Optional Material ---------------
--------------------------------------------------
--------------------------------------------------

-- Bonus L-Systems

peanoGosper :: Int -> Command
peanoGosper x = f x
    where 
      f 0 = GrabPen black :#: Go 7
      f x = f (x-1) :#: p :#: g (x-1) :#: p :#: p :#: g (x-1) :#: n :#: f (x-1) :#: n :#: n :#: f (x-1) :#: f (x-1) :#: n :#: g (x-1) :#: p
      g 0 = GrabPen green :#: Go 14
      g x = n :#: f (x-1) :#: p :#: g (x-1) :#: g (x-1) :#: p :#: p :#: g (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: n :#: g (x-1)
      n   = Turn 60
      p   = Turn (-60)

cross :: Int -> Command
cross x = f x
  where
    f 0 = GrabPen blue :#: n :#: GrabPen blue :#: n :#: GrabPen blue :#: n :#: GrabPen blue :#: n
    f x = f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n :#: f (x-1) :#: p :#: f (x-1)
    n   = Turn 90
    p   = Turn (-90)


branch :: Int -> Command
branch x = g x
    where 
      g 0 = GrabPen green :#: Go 10
      g x = f (x-1) :#: n :#: Branch( Branch( g (x-1)) :#: p :#: g (x-1)) :#: f (x-1) :#: Branch( p :#: f (x-1) :#: g (x-1)) :#: n :#: g (x-1)
      f 0 = GrabPen green :#: Go 5
      f x = f (x-1) :#: f (x-1)
      n   = Turn (-22.5)
      p   = Turn (22.5)

thirtytwo :: Int -> Command
thirtytwo x = f x
    where
      f 0 = GrabPen black :#: Go 10 :#: p :#: GrabPen black :#: Go 10 :#: p :#: GrabPen black :#: Go 10 :#: p :#: GrabPen black :#: Go 10
      f x = n :#: f (x-1) :#: p :#: f (x-1) :#: n :#: f (x-1)
            :#: n :#: f (x-1) :#: p
            :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n
            :#: f (x-1) :#: p :#: f (x-1)
            :#: p :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1)
            :#: n :#: f (x-1) :#: n :#: f (x-1) :#: n
            :#: f (x-1) :#: f (x-1) :#: p :#: f (x-1) --lb
            :#: f (x-1):#: n :#: f (x-1) :#: f (x-1)
            :#: p :#: f (x-1) :#: p :#: f (x-1) :#: n
            :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1) :#: n
            :#: f (x-1) :#: p :#: f (x-1) :#: f (x-1) :#: n :#: f (x-1)
            :#: n :#: f (x-1) :#: p :#: f (x-1) :#: p :#: f (x-1)
            :#: n :#: f (x-1) :#: p
      n   = Turn 90
      p   = Turn (-90)
main :: IO ()
main = do
    display (branch 5)
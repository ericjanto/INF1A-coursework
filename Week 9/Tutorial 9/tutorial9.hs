-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 9
--
-- Week 9(11-15 Nov.)
module Tutorial9 where

import Data.List
import Test.QuickCheck
import Data.Char


-- Type declarations

type FSM q = ([q], Alphabet, [q], [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      [0],
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      ['B'],
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [[0]],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> [q]
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]

states (k, _, _, _, _) = k
alph   (_, a, _, _, _) = a
start  (_, _, s, _, _) = s
final  (_, _, _, f, _) = f
trans  (_, _, _, _, t) = t


-- 2.
delta :: (Eq q) => FSM q -> [q] -> Char -> [q]
delta m s symbol = concat [lookUp state symbol transitions | state <- s]
    where
      transitions = trans m
      lookUp state symbol transitions = [g | (st,sym,g) <- transitions, st == state, sym == symbol]

test_delta = delta m1 [0] 'a'   == [1,2] &&
             delta m1 [1,2] 'a' == [3]   &&
             delta m1 [3] 'b'   == [4]   &&
             delta m2 ['B'] '0' == "A"

-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts m xs  =  acceptsFrom m (start m) xs

acceptsFrom :: (Eq q) => FSM q -> [q] -> String -> Bool
acceptsFrom m states ""     = or[ state `elem` final m | state <- states ]
acceptsFrom m states (x:xs) = acceptsFrom m (delta m states x) xs

test_acceptsFrom = acceptsFrom m1 [1, 2] "ab" == True  &&
                   acceptsFrom m1 [1] "ab"    == False &&
                   acceptsFrom m1 [2] "ab"    == True

-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical = sort . nub

test_canonical = canonical [3,5,66,7,5,4,5] == [3,4,5,7,66] &&
                 canonical [1,2]            == [1,2]        &&
                 canonical [2,1]            == [1,2]        &&
                 canonical [1,2,1]          == [1,2]

-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta m s symbol = canonical $ delta m s symbol

test_ddelta = ddelta m1 [0] 'b'   == [1,2] &&
              ddelta m1 [1,2] 'b' == [3,4] &&
              ddelta m1 [3,4] 'b' == [4]

-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next m ss = canonical $ [canonical (ddelta m states sym) | states <- ss, sym <- alph m] ++ ss

test_next = next m1 [[0]]                        == [[0],[1,2]]                  &&
            next m1 [[0],[1,2]]                  == [[0],[1,2],[3],[3,4]]        &&
            next m1 [[0],[1,2],[3],[3,4]]        == [[],[0],[1,2],[3],[3,4],[4]] &&
            next m1 [[],[0],[1,2],[3],[3,4],[4]] == [[],[0],[1,2],[3],[3,4],[4]]



-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable m ss = f 0 ss
    where
      f 0 ss = f 1 (next m ss)
      f 2 _  = error ":("
      f n ss | ss == f (n-1) ss = ss
             | otherwise        = f (n+1) (next m ss) -- problem is n+1 -> f is not defined for larger n's!

test_reachable = reachable m1 [[0]] == [[],[0],[1,2],[3],[3,4],[4]]

-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal m ss = filter (hasFinal m) ss

hasFinal :: (Ord q) => FSM q -> [q] -> Bool
hasFinal m ss = or [s `elem` final m | s <- ss]

test_dfinal = dfinal m1 [[],[0],[1,2],[3],[3,4],[4]] == [[3,4],[4]]

-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans m ss = [(s, sym, ddelta m s sym) | s <- ss, sym <- alph m]

test_dtrans = dtrans m1 [[],[0],[1,2],[3],[3,4],[4]] == 
            [([],'a',[]),
             ([],'b',[]),
             ([0],'a',[1,2]),
             ([0],'b',[1,2]),
             ([1,2],'a',[3]),
             ([1,2],'b',[3,4]),
             ([3],'a',[]),
             ([3],'b',[4]),
             ([3,4],'a',[4]),
             ([3,4],'b',[4]),
             ([4],'a',[4]),
             ([4],'b',[4])]

-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic m = (states,
                  alph m,
                  [start m],
                  dfinal m states,
                  dtrans m states)
                  where
                    states = reachable m [start m]

test_deterministic = deterministic m1 == dm1


-- Optional Material

-- QuickCheck

safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

--11.

charFSM :: Char -> FSM Bool
charFSM x = ([True,False],
             ['a'..'z'],
             [False],
             [True],
             [(False,x,True)])

emptyFSM :: FSM ()
emptyFSM = ([()],
            ['a'..'z'],
            [()],
            [()],
            [])

--12.

concatFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Either q q')
concatFSM a b = undefined

prop_concatFSM :: String -> String -> String -> Bool
prop_concatFSM m n o =
  accepts fsm (s ++ t)
  && (accepts fsm u == (s ++ t == u))
  where
  fsm = concatFSM a b
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o

--13.

intFSM :: Ord q => FSM q -> FSM Int
intFSM a = undefined

lookUp :: Eq q =>  q -> [(q,Int)] ->  Int
lookUp q' qis =  the [ i | (q,i) <- qis, q == q' ]
  where
  the [q] = q

stringFSM :: String -> FSM Int
stringFSM = undefined

prop_stringFSM m n =
  accepts a s
  && accepts a t == (s == t)
  where
  a = stringFSM s
  s = safeString m
  t = safeString n

--14.

completeFSM :: (Ord q) => FSM q -> FSM (Maybe q)
completeFSM  = undefined

unionFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Maybe q, Maybe q')
unionFSM = undefined
        
prop_unionFSM :: String -> String -> String -> Bool
prop_unionFSM m n o =
  accepts fsm u == (accepts a u || accepts b u)
  && accepts fsm s
  && accepts fsm t
  where
  fsm = unionFSM a b
  a = stringFSM s
  b = stringFSM t
  c = stringFSM u
  s = safeString m
  t = safeString n
  u = safeString o

--15.

star :: (Ord q) => FSM q -> FSM q
star = undefined

prop_star :: String -> Int -> Bool
prop_star m n =
  accepts fsm (concat (replicate i s))
  where
  fsm = star (stringFSM s)
  s = safeString m
  i = abs n

--16.

complementFSM :: (Ord q) => FSM q -> FSM (Maybe q)
complementFSM = undefined
           
prop_complement :: String -> String -> Bool
prop_complement m n =
  not (accepts fsm s)
  && accepts fsm t == not (s == t)
  where
  fsm = complementFSM (stringFSM s)
  s = safeString m
  t = safeString n

intersectFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (q,q')
intersectFSM = undefined
                
prop_intersectFSM1 m n =
  accepts fsm s
  && accepts fsm t == (s == t)
  where
  fsm = intersectFSM a a
  a = stringFSM s
  s = safeString m
  t = safeString n

prop_intersectFSM2 m n o =
  accepts fsm u == (accepts a u && accepts b u)
  where
  fsm = intersectFSM a b
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o

prop_intersectFSM3 m n o =
  accepts fsm s
  && accepts fsm u == accepts a u
  where
  fsm = intersectFSM a (unionFSM a b)
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o

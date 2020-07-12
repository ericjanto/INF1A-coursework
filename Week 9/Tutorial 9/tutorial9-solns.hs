-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 9
--
-- Week 9(11-15 Nov.)
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!

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
        "ab",
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
delta m s symbol = [ q1 | (q0, x, q1) <- trans m, q0 `elem` s, x == symbol ]


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts m xs = acceptsFrom m (start m) xs

acceptsFrom :: (Eq q) => FSM q -> [q] -> String -> Bool
acceptsFrom m q "" = or[ r `elem` final m | r <- q ]
acceptsFrom m q (x:xs) = acceptsFrom m (delta m q x) xs


-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical = nub . sort


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta m superq symbol =
    canonical ( delta m superq symbol )

-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next m superqs = canonical (
     superqs ++ [ddelta m superq symbol | superq <- superqs, symbol <- alph m])


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable m superqs = if superqs' == superqs then superqs
                      else reachable m superqs'
                          where
                            superqs' = next m superqs

-- 8.
containsFinal m superq = or [finalq == q | finalq <- final m, q <- superq]

dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal m superqs = filter (containsFinal m) superqs

-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans m superqs = 
    [(sq, symbol, ddelta m sq symbol) | sq <- superqs, symbol <- alph m]


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic m = (states, alph m, [start m], dfinal m states, dtrans m states)
  where states = reachable m [start m]

-- Optional Material

-- QuickCheck

safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

--11.

charFSM :: Char -> FSM Bool
charFSM c =
  ([False,True],
   ['a'..'z'],
   [False],
   [True],
   [(False,c,True)])

emptyFSM :: FSM ()
emptyFSM =
  ([()],
   ['a'..'z'],
   [()],
   [()],
   [])

-- 12.
mapTrans :: (q -> q') -> [Transition q] -> [Transition q']
mapTrans f trs  =  [ (f q, ch, f q') | (q, ch, q') <- trs ]

concatFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Either q q')
concatFSM a b =
  (map Left (states a) ++ map Right (states b),
   nub (alph a ++ alph b),
   map Left (start a),
   map Left (if or[q`elem` final b| q <- start b] then final a else []) ++ map Right (final b),
   mapTrans Left (trans a)
   ++ mapTrans Right (trans b)
   ++ [(Left q, char, Right q') | q <- final a, char <- alph b, q' <- delta b (start b) char])

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

-- 13.

intFSM :: Ord q => FSM q -> FSM Int
intFSM a = undefined
  ([0..length (states a) -1],
   alph a,
   map (`lookUp` intMap) (start a),
   map (`lookUp` intMap) (final a),
   mapTrans (`lookUp` intMap) (trans a))
  where
  intMap = zip (nub (states a)) [0..length(states a) -1]

lookUp :: Eq q =>  q -> [(q,Int)] ->  Int
lookUp q' qis =  the [ i | (q,i) <- qis, q == q' ]
  where
  the [q] = q

stringFSM :: String -> FSM Int
stringFSM  =  foldr iconcatFSM iemptyFSM . map icharFSM
  where
  iconcatFSM a b = intFSM (concatFSM a b)
  iemptyFSM      = intFSM emptyFSM
  icharFSM c     = intFSM (charFSM c)

prop_stringFSM m n =
  accepts a s
  && accepts a t == (s == t)
  where
  a = stringFSM s
  s = safeString m
  t = safeString n

-- 14.
completeFSM :: (Ord q) => FSM q -> FSM (Maybe q)
completeFSM a =
  (map Just (states a) ++ [Nothing],
   alph a,
   map Just (start a),
   map Just (final a),
   mapTrans Just (trans a)
   ++ [ (Just q, c, Nothing) | q <- states a, c <- alph a, null (delta a [q] c) ]
   ++ [ (Nothing, c, Nothing) | c <- alph a ])

unionFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Maybe q, Maybe q')
unionFSM a b = unionFSMhelp (completeFSM a) (completeFSM b)
  where
  unionFSMhelp :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (q,q')
  unionFSMhelp a b =
    ([ (q,r) | q <- states a, r <- states b],
     nub (alph a ++ alph b),
     [ (q,r) | q <- start a, r <- start b],
     [ (q,r) | q <- states a, r <- states b, q `elem` final a || r `elem` final b],
     [ ((q,r), c, (q',r')) | (q,c,q') <- trans a, (r,d,r') <- trans b, c == d])
        
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
star a =
  (states a,
   alph a,
   start a,
   canonical (start a ++ final a),
   trans a ++ [(f, c, q) | c <- alph a, f <- final a, q <- delta a (start a) c])

prop_star :: String -> Int -> Bool
prop_star m n =
  accepts fsm (concat (replicate i s))
  where
  fsm = star (stringFSM s)
  s = safeString m
  i = abs n

--16.

complementFSM :: (Ord q) => FSM q -> FSM (Maybe q)
complementFSM a = complementHelp (completeFSM a)
  where
  complementHelp :: (Ord q) => FSM q -> FSM q
  complementHelp a =
    (states a,
     alph a,
     start a,
     states a \\ final a,
     trans a)
           
prop_complement :: String -> String -> Bool
prop_complement m n =
  not (accepts fsm s)
  && accepts fsm t == not (s == t)
  where
  fsm = complementFSM (stringFSM s)
  s = safeString m
  t = safeString n

intersectFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (q,q')
intersectFSM a b =
  ([ (q,r) | q <- states a, r <- states b],
   alph a,
   [ (q,r) | q <- start a, r <- start b],
   [ (q,r) | q <- final a, r <- final b],
   [((q,r), c, (q',r')) | (q,c,q') <- trans a, (r,d,r') <- trans b, c == d])
                
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

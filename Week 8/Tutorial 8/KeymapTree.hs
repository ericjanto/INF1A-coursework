-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 8
--
-- Week 8(04-08 Nov.)
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, del,
                    select,
                    toList, fromList,
                    merge, filterLT, filterGT                  
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + maximum (depth left, depth right)

test_sd = size testTree  == 4 &&
          depth testTree == 3

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k v Leaf Leaf) = [(k,v)]
toList (Node k v left@(Node kL _ _ _) right@(Node kR _ _ _))
    | kL < k && k < kR = toList left ++ ((k,v) : toList right)
    | kL < k && k > kR = toList left ++ toList right ++ [(k,v)]
    | k  < kL          = (k,v) : toList left ++ toList right
toList (Node k v Leaf right@(Node kR _ _ _))
    | k < kR = (k,v) : toList right
    | k > kR = toList right ++ [(k,v)]
toList (Node k v left@(Node kL _ _ _) Leaf)
    | k < kL = (k,v) : toList left
    | k > kL = toList left ++ [(k,v)]

-- Exercise 8 

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <  k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key = f
    where
      f Leaf  = Nothing
      f (Node k v left right) | key == k  = Just v
                              | key <  k  = f left
                              | otherwise = f right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList = foldr (uncurry set) emptyTree
    where
      emptyTree = Leaf


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 13

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT k t = f t
    where
      f Leaf = Leaf
      f (Node key v left right)
          | key < k   = (Node key v left (f right))
          | key >= k  = f left

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT k t = f t
    where
      f Leaf = Leaf
      f (Node key v left right)
          | key > k    = (Node key v (f left) right)
          | key <= k   = f right

-- Exercise 14

merge :: (Ord k) => Keymap k a -> Keymap k a -> Keymap k a
merge t1 t2 = fromList $ nubBy p (toList t1 ++ toList t2)
    where
      p (k,_) (k',_) = k == k'

prop_merge :: Keymap Int Int -> Keymap Int Int -> Bool                
prop_merge t1 t2 = sort (nubBy p (toList t1 ++ toList t2)) == toList (merge t1 t2)
  where p (k,_) (k',_) = k == k'

-- Exercise 15

del :: (Ord k) => k -> Keymap k a -> Keymap k a
del k t = f t
    where
      f Leaf = Leaf
      f (Node key v l r)
        | k == key  = merge l r
        | k < key   = Node key v (f l) r
        | k > key   = Node key v l (f r)


-- Exercise 16

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select p tree = f tree
    where
      f Leaf = Leaf
      f (Node k v l r) | p v       = (Node k v (f l) (f r))
                       | otherwise = select p (del k tree)


select' :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select' _ Leaf = Leaf
select' f (Node k v left right) 
    | f v       = Node k v (select' f left) (select' f right)
    | otherwise = merge (select' f left) (select' f right) 

prop_select :: (Ord k, Eq a) => (a -> Bool) -> Keymap k a -> Bool
prop_select p t = toList(select p t) == toList(select' p t)

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 8
--
-- Week 8(04-08 Nov.)

-- ghci
-- :set -package random
-- :l tutorial8

module Tutorial8 where

import System.Random


-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen xs = maximum [ length name | (b,(name,u)) <- xs]

formatLine :: Int -> (Barcode, Item) -> String
formatLine x (c,(p,u)) = c ++ take 3 dots ++ fillUp p ++ take 3 dots ++ u
    where
      fillUp s = s ++ take (x - length s) dots
      dots = repeat '.'

test_formatLine = formatLine 7 ("0001",("product","unit"))  == "0001...product...unit" &&
                  formatLine 7 ("0002",("thing","unknown")) == "0002...thing.....unknown"


-- 1. Db -> toList
-- 2. Find longest product name
-- 3. Pretty print according to longest len

showCatalogue :: Catalogue -> String
showCatalogue ct = concat $ map addBreak (map (formatLine y) (toList ct))
    where
      y          = longestProductLen (toList ct)
      addBreak s = s ++ "\n"
    
     
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing   = []
maybeToList (Just x)  = [x] 

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

catMaybes :: [Maybe a] -> [a]
catMaybes xs = concat $ map maybeToList xs

-- Exercise 3

-- 1. Get for each barcode the corresponding item with get (:t get for info)
-- 2. catMaybe the result

getItems :: [Barcode] -> Catalogue -> [Item]
getItems bcs ct = catMaybes [get bc ct| bc <- bcs]


-- Exercise 4

-- a. (0.72 secs, 874,745,080 bytes)
-- b. 
   {- 1. 0.11s
      2. 0.08s
      3. 0.02s
      4. 0.02s
      5. 0.01s
      6. 0.02s
      7. 0.01s
      8. 0.04s
      9. 0.03s
     10. 0.02s
   -}

{- 

It would take on average twice as much time.

The get function looks at (n-1) items if the item is the last one of a
db with size n.

-}
 
-- For Exercises 6-10 check KeymapTree.hs 

-- Exercise 11

-- a. 4.51s (as List: 0.61s)
-- b.
{- 1. 0.00s
   2. 0.00s
   3. 0.00s
   4. 0.00s
   5. 0.00s
   6. 0.00s
   ... that's incredible
-}

-- Exercise 12 -> KeyMapTree.hs

-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)

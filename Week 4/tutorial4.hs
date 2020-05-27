-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 4
--
-- Week 4(07-11 Oct.)

module Tutorial4 where

import Data.List
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:irene.vp@ed.ac.uk\">Irene Vlassi</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/A/testpage.html\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br><b>TA:</b> "
            , "mailto:irene.vp@ed.ac.uk\">Irene Vlassi</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Philip Wadler","wadler@inf.ed.ac.uk")
               , ("Irene Vlassi","irene.vp@ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString s1 s2 = map toLower s1 == map toLower s2


-- 2.
prefix :: String -> String -> Bool
prefix s1 s2 = map toLower s1 `isPrefixOf` map toLower s2


prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n =  prefix substr (map toLower str) &&
                         prefix substr (map toUpper str)
                           where
                             substr  =  take n str

prop_prefix_neg :: String -> Int -> Bool
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
                          where substr = take n str
        
        
-- 3.
contains :: String -> String -> Bool
contains _ [] = True
contains [] _ = False
contains s1@(x:xs) s2
    | s2 `prefix` s1 = True
    | otherwise          = contains xs s2

prop_contains :: String -> Int -> Int -> Bool
prop_contains str n m = (map toUpper str) `contains` substr &&
                        (map toLower str) `contains` substr
                              where
                                substr = take n (drop m str)


-- 4.
takeUntil :: String -> String -> String
takeUntil [] s2    = s2
takeUntil _ []     = []
takeUntil s1 s2@(x:xs)
    | prefix s1 s2 = []
    | otherwise    = x : takeUntil s1 xs

dropUntil :: String -> String -> String
dropUntil _ []  = []
dropUntil [] s2 = s2
dropUntil s1 s2 = drop indexLstCh s2
    where
      indexLstCh = length (takeUntil s1 s2) + length s1


-- 5.
split :: String -> String -> [String]
split [] s  = error "Cannot split with an empty list. You suck!"
split _ []  = []
split sep s = takeUntil sep s : split sep (dropUntil sep s)

reconstruct :: String -> [String] -> String
reconstruct _ []  = []
reconstruct sep [s] = s
reconstruct sep (x:xs) = x ++ sep ++ reconstruct sep xs 

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
    where
      sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML s = split "<a href=\"" cleaned
    where
      cleaned = dropUntil "<a href=\"" s

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails ls = filter p ls
    where
      p l = takeUntil ":" l `sameString` "mailto"


-- 8.
link2pair :: Link -> (Name, Email)
link2pair l = (name, email)
    where
      name  = takeUntil "<" $ dropUntil ">" l
      email = takeUntil "\"" $ dropUntil ":" l


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML html = nub pairs
    where
      pairs = map link2pair emails
      emails = takeEmails $ linksFromHTML html
      

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail n l = filter p l
    where
      p (name,email) = contains name n

-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html n = findEmail n emails 
    where emails = emailsFromHTML html


-- Optional Material

-- 13.
hasInitials :: String -> Name -> Bool
hasInitials s n = f s (split " " n)
  where
    f :: String -> [String] -> Bool
    f [] _ = True
    f _ [] = True
    f (x:xs) (name : names)
      | x == head name = f xs names
      | otherwise      = False

-- 14.
emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name, Email)]
emailsByMatchFromHTML p html = f p mails
    where
      mails = emailsFromHTML html
      f :: (Name -> Bool) -> [(Name, Email)] -> [(Name, Email)]
      f _ [] = []
      f p (pair@(name,email):xs)
          | p name    = pair : f p xs
          | otherwise = f p xs 

emailsByInitialsFromHTML :: String -> HTML -> [(Name, Email)]
emailsByInitialsFromHTML s html = emailsByMatchFromHTML p html
    where
      p = hasInitials s

-- 15.

-- If your criteria use parameters (like hasInitials), change the type signature.
myCriteria :: Name -> Bool
myCriteria = (== "Irene")   

emailsByMyCriteriaFromHTML :: HTML -> [(Name, Email)]
emailsByMyCriteriaFromHTML = emailsByMatchFromHTML myCriteria

-- 16.
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = concat [ aft name ++ ", " ++ pre name ++ "\t\t" ++ email ++ "\n" | (name,email) <- addr ]
    where
      aft name = last $ split " " name
      pre name = head $ split " " name

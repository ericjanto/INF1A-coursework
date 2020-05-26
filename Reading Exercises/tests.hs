module Tests where

import Data.Char
import Data.List
import Test.QuickCheck

main = do
  putStrLn "hello eric"

-- Functions
universe :: Int -> String
universe 42 = "It was quite obvious, wasn't it?"
universe _  = "Are you stupid?"

firstLetters :: String -> String
firstLetters all@(x:y:xs) = "Input: '" ++ all
                         ++ "'. First two letters: '"
                         ++ x : "' and '"
                         ++ y : "'"

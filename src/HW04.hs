module HW04 where

import Data.Maybe()
import Data.Char
import Data.List
import Control.Monad

import BST

insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _   value Leaf = Node Leaf value Leaf
insertBST cmp value (Node left center right)
    | c == GT   = Node left center (insertBST cmp value right)
    | otherwise = Node (insertBST cmp value left) center right
    where c = value `cmp` center

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

allCaps :: [String] -> Bool
allCaps x = all (maybe False isUpper) (map safeHead x)

dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = foldr backwardCat ""
    where backwardCat ch str
              | null str == False = ch:str
              | ch == ' '         = ""
              | otherwise         = [ch]

firstLetters :: [String] -> [Char]
firstLetters strings = [ x | (x:_) <- strings]

asList :: [String] -> String
asList list = "[" ++ (intercalate "," list) ++ "]"

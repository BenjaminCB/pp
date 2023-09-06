module Lecture1 where

main :: IO ()
main = print $ mySum myBigOak

myLength :: [a] -> Int
myLength = foldr (\_ a -> a + 1) 0

-- This is the simple program from the slides from the introduction

laengde :: (Num p) => [a] -> p
laengde = foldr (\_ a -> a + 1) 0

myList :: [Int]
myList = [2, 3, 17, 9, 69, 484000]

data BTree a = BLeaf a | BBranch a (BTree a) (BTree a) deriving (Show)

instance Functor BTree where
    fmap f (BLeaf x) = BLeaf (f x)
    fmap f (BBranch x t1 t2) = BBranch (f x) (f <$> t1) (f <$> t2)

instance Foldable BTree where
    foldr f z (BLeaf x) = f x z
    foldr f z (BBranch x t1 t2) = f x (foldr f (foldr f z t1) t2)

mySum :: (Foldable t, Num a) => t a -> a
mySum = foldr (+) 0

sumtree :: (Num a) => BTree a -> a
sumtree = mySum

myBigOak :: BTree Int
myBigOak = BBranch 14 (BLeaf 13) (BLeaf 17)

-- Quicksort
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = small ++ [x] ++ big
    where
        small = filter (<= x) xs
        big = filter (> x) xs

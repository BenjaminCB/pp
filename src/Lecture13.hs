module Lecture13 where

data Tree = Leaf | Node Tree Tree deriving Show
data Direction = L | R deriving Show
type Path = [Direction]

main :: IO ()
main = do
    -- print $ take 10 $ plip 3
    -- print $ take 10 $ plip' 3
    -- print $ take 10 $ plip'' 3
    -- print $ take 10 $ plip''' 3
    -- print $ take 50 $ fibFrom 0 1
    print $ take 20 allBinaries
    -- print $ take 20 fib
    -- print $ head $ fletind 1 (2: undefined)
    -- print $ fletind 1 [2..10]
    -- print $ fletind 1 [2]
    -- print $ take 10 numList

plip :: Int -> [Int]
plip n = [0, n..]

plip' :: Int -> [Int]
plip' n = let g m = m : g (m + n) in g 0

plip'' :: Int -> [Int]
plip'' = flip map [0..] . (*)

plip''' :: Int -> [Int]
plip''' = zipWith (*) [0..] . repeat

fibFrom :: Int -> Int -> [Int]
fibFrom a b = a : fibFrom b (a + b)

binaryStep :: [[Bool]] -> [[Bool]]
binaryStep [] = []
binaryStep (xs:xss) = (False : xs) : (True : xs) : binaryStep xss

allBinaries :: [[Bool]]
allBinaries = [False] : [True] : allBinaries' [[False, True], [True, True]]
    where
        allBinaries' :: [[Bool]] -> [[Bool]]
        allBinaries' xss = xss ++ allBinaries' (binaryStep xss)

binaryToInteger :: [Bool] -> Int
binaryToInteger = foldr (\x y -> if x then 1 + 2 * y else 2 * y) 0

fib :: [Int]
fib = zipWith (+) (0 : 1 : fib) (1 : fib)

len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

fletind :: a -> [a] -> [a]
fletind _ [] = []
fletind x (y:ys) = y : x : fletind x ys

intersperse :: [a] -> [a] -> [a]
intersperse [] ys = ys
intersperse xs [] = xs
intersperse (x:xs) (y:ys) = x : y : intersperse xs ys

allFinitePaths :: Tree -> [Path]
allFinitePaths Leaf = [[]]
allFinitePaths (Node l r) = map (L:) (allFinitePaths l) `intersperse` map (R:) (allFinitePaths r)

numList :: [Int]
numList = let ns (x:xs) = x : ns (filter (`elem` [2 * x, 3 * x,5 * x]) xs)
           in ns [1..]

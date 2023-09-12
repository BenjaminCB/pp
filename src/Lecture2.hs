module Lecture2 where

import Prelude hiding (product)

main :: IO ()
main = do
    putStrLn $ "product [] = " ++ show (product [])
    putStrLn $ "product [1..5] = " ++ show (product [1 .. 5])
    putStrLn $ "second [1,4,5,6] = " ++ show (second [1, 4, 5, 6])
    putStrLn $ "second [\"some\", \"bizzare\", \"mango\"] = " ++ show (second ["some", "bizzare", "mango"])
    putStrLn $ "allButSecond [1,4,5,6] = " ++ show (allButSecond [1, 4, 5, 6])
    putStrLn $ "midtover [1..5] = " ++ show (midtover [1 .. 5])
    putStrLn $ "qsDesc [1,5,4,3,6] = " ++ show (qsDesc [1, 5, 4, 3, 6])

-- prep sheet says to use 0 for empty list, but that does not make any sense to me
product :: (Num a) => [a] -> a
product [] = 0
product [x] = x
product (x : xs) = x * product xs

second :: [a] -> a
second [] = undefined
second [_] = undefined
second (_ : x : _) = x

allButSecond :: [a] -> [a]
allButSecond [] = []
allButSecond [x] = [x]
allButSecond (x : _ : xs) = x : xs

midtover :: [a] -> ([a], [a])
midtover xs = (take mid xs, drop mid xs)
    where
        mid = length xs `div` 2

final :: [a] -> a
final = head . reverse

sortBy :: (Ord b) => (a -> b) -> [a] -> [a]
sortBy f [] = []
sortBy f (x : xs) = lt ++ [x] ++ gt
    where
        lt = filter (\y -> f y <= f x) xs
        gt = filter (\y -> f y > f x) xs

qsDesc :: (Ord a) => [a] -> [a]
qsDesc [] = []
qsDesc (x : xs) = qsDesc gt ++ [x] ++ qsDesc lt
    where
        lt = filter (<= x) xs
        gt = filter (> x) xs

module Lecture5 where

import Prelude hiding (replicate, repeat, reverse, last, any, filter)

main :: IO ()
main = do
    print $ reverse [1..5]
    print $ wrapup [1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 5, 5]
    print $ rie [1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 5, 5]
    print $ frequencies [1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 5, 5, 1, 1]
    print $ cfracn 10 (sqrt 2)

repeat :: a -> [a]
repeat x = x : repeat x

replicate :: Int -> a -> [a]
replicate n = take n . repeat

improve :: [a] -> [a]
improve [] = []
improve [x] = [x]
improve (x : _ : xs) = x : improve xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

last :: [a] -> a
last [] = undefined
last [x] = x
last (_:xs) = last xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs)
    | f x = x : filter f xs
    | otherwise = filter f xs

isolate :: Eq a => a -> [a] -> ([a], [a])
isolate a xs = (filter (/= a) xs, filter (== a) xs)

wrapup :: Eq a => [a] -> [[a]]
wrapup [] = []
wrapup [x] = [[x]]
wrapup (x:xs) = (x : takeWhile (== x) xs) : wrapup (dropWhile (== x) xs)

triples :: Num a => [(a, a, a)] -> ([a], [a], [a])
triples xs = (map (\(x, _, _) -> x) xs, map (\(_, x, _) -> x) xs, map (\(_, _, x) -> x) xs)

rie :: Eq a => [a] -> [(a, Int)]
-- could be fancy and use liftA2 (,) head length
rie xs = map (\x -> (head x, length x)) (wrapup xs)

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x:xs) = f x || any f xs

frequencies :: Eq a => [a] -> [(a, Int)]
frequencies [] = []
frequencies (x:xs) = (x, length identical + 1) : frequencies different
    where (different, identical) = isolate x xs

cfrac :: Double -> [Int]
cfrac x = fx : cfrac (1 / (x - fromIntegral fx))
  where
    fx = floor x

cfracn :: Int -> Double -> [Int]
cfracn n = take n . cfrac

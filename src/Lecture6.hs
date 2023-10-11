module Lecture6 where

import Control.Applicative (liftA2)

main :: IO ()
main = do
    print $ prep1 "abba"
    print $ prep2 9
    print $ within [1 .. 10] (3, 7)
    print $ sumrows [[1, 2, 3], [4, 5, 6], [7, 8, 9], []]
    print $ fact 5
    print $ approx 10
    print $ fingo [1, 2, 3] [4, 5, 6]
    print $ fingo [1, 2, 3] $ fingo [4, 5, 6] [7, 8, 9]
    print $ partition (< 5) [1 .. 10]
    print $ partition' (< 5) [1 .. 10]
    print $ filter' (< 5) [1 .. 10]
    print $ remove [1, 3, 5] [1 .. 10]
    print $ min2 [1 .. 10]
    print $ min2 [1, 1, 2]

prep1 :: String -> [Int]
prep1 = map (flip (-) 96 . fromEnum)

prep2 :: (Integral a) => Int -> a
prep2 = foldr (\a b -> a ^ 2 + b) 0 . flip take [1 ..]

within :: (Ord a) => [a] -> (a, a) -> [a]
within xs (lt, gt) = filter (liftA2 (&&) (>= lt) (<= gt)) xs

sumrows :: [[Int]] -> [Int]
sumrows = map sum

fact :: (Enum a, Num a) => Int -> a
fact = product . flip take [1 ..]

approx :: (Enum a, Fractional a) => Int -> a
approx = sum . map ((1 /) . fact) . flip take [0 ..]

fingo :: [a] -> [a] -> [a]
fingo = foldr (:) -- flip (++)

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p = foldr (\a (xs, ys) -> if p a then (a : xs, ys) else (xs, a : ys)) ([], [])

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p xs = (filter p xs, filter (not . p) xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\a b -> if p a then a : b else b) []

remove :: (Eq a) => [a] -> [a] -> [a]
remove xs = foldr (\a b -> if a `elem` xs then b else a : b) []

min2 :: (Ord a) => [a] -> a
min2 [] = undefined
min2 [_] = undefined
min2 (x : y : xs) = uncurry max $ foldr f (min (x, y) (y, x)) xs
    where
        f a (b1, b2)
            | a < b1 = (a, b1)
            | a < b2 = (b1, a)
            | otherwise = (b1, b2)

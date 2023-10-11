module Lecture4 where

main :: IO ()
main = do
    print $ allDots [(1, 2), (3, 4)] [(5, 6), (7, 8)]
    print $ allDotsGen [(1, 2), (3, 4)] [(5, 6), (7, 8)]
    print $ sevens 100
    print $ sevens 0
    print $ pyt 10
    print $ pyt 0
    print $ pyt 1
    print $ isPerfect 28

onlyTwo :: [a] -> Bool
onlyTwo [_, _] = True
onlyTwo _ = False

dot :: (Num a) => (a, a) -> (a, a) -> a
dot (a, b) (c, d) = a * c + b * d

allDots :: (Num a) => [(a, a)] -> [(a, a)] -> [a]
allDots xs ys = [dot x y | x <- xs, y <- ys]

allDotsGen :: (Applicative f, Num a) => f (a, a) -> f (a, a) -> f a
allDotsGen xs ys = dot <$> xs <*> ys

sevens :: (Integral a) => a -> [a]
sevens n = [x | x <- [1 .. n - 1], x `mod` 7 == 0]

pyt :: (Integral a) => a -> [(a, a, a)]
pyt n =
    [ (x, y, z) | x <- [1 .. n - 1], y <- [1 .. n - 1], x < y, z <- [1 .. n], y < z, x ^ 2 + y ^ 2 == z ^ 2
    ]

headsup :: (Eq a) => [a] -> Bool
headsup (x : y : _) = x == y
headsup _ = False

plonk :: (Num a) => a -> a -> a -> a
plonk = \x y z -> x + y + z

flop :: [(a, b)] -> [(b, a)]
flop = map (\(x, y) -> (y, x))

dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = x : x : dupli xs

isPerfect :: (Integral a) => a -> Bool
isPerfect n = n == sum [x | x <- [1 .. n - 1], n `mod` x == 0]

bighead :: (Ord a) => [a] -> [a]
bighead [] = []
bighead (x : xs) = [y | y <- xs, y > x]

sums :: (Num a, Enum a) => a -> a -> [a]
sums n m = (+) <$> [1 .. n] <*> [1 .. m]

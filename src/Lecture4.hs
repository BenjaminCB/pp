module Lecture4 where

main :: IO ()
main = putStrLn "Hello, World!"

onlyTwo :: [a] -> Bool
onlyTwo [_,_] = True
onlyTwo _     = False

allDots :: Num a => [(a,a)] -> [(a,a)] -> [a]
allDots xs ys = [ a * c + b * d | (a,b) <- xs, (c,d) <- ys ]

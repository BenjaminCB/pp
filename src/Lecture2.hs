module Lecture2 where

import Prelude hiding (product)

main :: IO ()
main = do
    putStrLn $ "product [1..5] = " ++ show (product [1..5])

-- prep sheet says to use 0 for empty list, but that does not make any sense to me
product :: (Num a) => [a] -> a
product [] = 1
product (x : xs) = x * product xs

second :: [a] -> a
second [] = undefined
second [_] = undefined
second (_ : x : _) = x

module Lecture2 where

import Prelude hiding (product)

main :: IO ()
main = do
    putStrLn $ "product [] = " ++ show (product [])
    putStrLn $ "product [1..5] = " ++ show (product [1..5])
    putStrLn $ "second [1,4,5,6] = " ++ show (second [1,4,5,6])
    putStrLn $ "second [\"some\", \"bizzare\", \"mango\"] = " ++ show (second ["some", "bizzare", "mango"])
    putStrLn $ "second [1] = " ++ show (second [1])

-- prep sheet says to use 0 for empty list, but that does not make any sense to me
product :: (Num a) => [a] -> a
product [] = 1
product (x : xs) = x * product xs

second :: [a] -> a
-- second [] = undefined
-- second [_] = undefined
second (_ : x : _) = x

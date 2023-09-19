module Lecture3 where

main :: IO ()
main = print . take 10 $ guango 1

guango :: a -> [a]
guango a = a : guango a

tango :: Num n => (a, b) -> m -> n
tango _ _ = 0

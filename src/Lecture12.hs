module Lecture12 where

import Parsing

data Onion = Core Int | Layer Onion deriving Show

main :: IO ()
main = do
    putStrLn "theonion"
    print $ parse theonion "1"
    print $ parse theonion "L1"
    print $ parse theonion "L2"
    print $ parse theonion "hello"
    print $ parse theonion "LLL3hello"
    putStrLn "theonion'"
    print $ parse theonion' "1"
    print $ parse theonion' "L1"
    print $ parse theonion' "L2"
    print $ parse theonion' "hello"
    print $ parse theonion' "LLL3hello"
    putStrLn "theonion''"
    print $ parse theonion'' "1"
    print $ parse theonion'' "L1"
    print $ parse theonion'' "L2"
    print $ parse theonion'' "hello"
    print $ parse theonion'' "LLL3hello"
    putStrLn "theonion'''"
    print $ parse theonion''' "1"
    print $ parse theonion''' "L1"
    print $ parse theonion''' "L2"
    print $ parse theonion''' "hello"
    print $ parse theonion''' "LLL3hello"
    putStrLn "ab"
    print $ parse ab "ab"
    print $ parse ab "aabb"
    print $ parse ab "aaabbb"
    print $ parse ab "aaababbb"
    putStrLn "ab'"
    print $ parse ab' "ab"
    print $ parse ab' "aabb"
    print $ parse ab' "aaabbb"
    print $ parse ab' "aaababbb"

theonion :: Parser Onion
theonion = Layer <$> (char 'L' *> theonion) <|> Core <$> nat

theonion' :: Parser Onion
theonion' = do
    char 'L'
    o <- theonion'
    return $ Layer o
    <|> do
        n <- nat
        return $ Core n

theonion'' :: Parser Onion
theonion'' = do
    char 'L'
    Layer <$> theonion''
    <|>
        Core <$> nat

theonion''' :: Parser Onion
theonion''' = do
    ls <- many $ char 'L'
    n <- nat
    return $ foldr (\_ o -> Layer o) (Core n) ls

ab :: Parser String
ab = (\middle -> "a" ++ middle ++ "b") <$> (char 'a' *> ab <* char 'b') <|> return ""

ab' :: Parser String
ab' = do
    char 'a'
    middle <- ab'
    char 'b'
    return $ "a" ++ middle ++ "b"
    <|> return ""

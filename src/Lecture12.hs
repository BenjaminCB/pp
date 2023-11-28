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
    putStrLn "regexp"
    print $ parse regexp "a"
    print $ parse regexp "a|b"
    print $ parse regexp "a|b*"
    print $ parse regexp "a|ba"
    print $ parse regexp "(a|b)a"
    print $ parse regexp "a|(b*)"
    print $ parse regexp "a|(b*)*"
    putStrLn "match"
    print $ parse (match $ C_L1 $ C_L2 $ C_L3 $ C_L4 A) "a"
    print $ parse (match $ C_L1 $ C_L2 $ C_L3 $ C_L4 A) "b"
    print $ parse (match $ Union (C_L2 $ C_L3 $ C_L4 A) (C_L1 $ C_L2 $ C_L3 $ C_L4 B)) "a"
    print $ parse (match $ Union (C_L2 $ C_L3 $ C_L4 A) (C_L1 $ C_L2 $ C_L3 $ C_L4 B)) "b"
    print $ parse (match $ C_L1 $ Concat (C_L3 $ C_L4 A) (C_L2 $ C_L3 $ C_L4 B)) "ab"
    print $ parse (match $ C_L1 $ C_L2 $ Star $ C_L4 A) "a"
    print $ parse (match $ C_L1 $ C_L2 $ Star $ C_L4 A) "aaaab"

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

data RegExp = Union L1 RegExp | C_L1 L1 deriving Show
data L1 = Concat L2 L1 | C_L2 L2 deriving Show
data L2 = Star L3 | C_L3 L3 deriving Show
data L3 = Paren RegExp | C_L4 L4 deriving Show
data L4 = A | B deriving Show

regexp :: Parser RegExp
regexp = do
    l1' <- l1
    char '|'
    Union l1' <$> regexp
    <|> C_L1 <$> l1

l1 :: Parser L1
l1 = do
    l2' <- l2
    Concat l2' <$> l1
    <|> C_L2 <$> l2

l2 :: Parser L2
l2 = do
    l3' <- l3
    char '*'
    return $ Star l3'
    <|> C_L3 <$> l3

l3 :: Parser L3
l3 = do
    char '('
    r <- regexp
    char ')'
    return $ Paren r
    <|> C_L4 <$> l4

l4 :: Parser L4
l4 = do
    char 'a'
    return A
    <|> do
        char 'b'
        return B

match :: RegExp -> Parser String
match (C_L1 l1') = matchL1 l1'
match (Union l1' r) = matchL1 l1' <|> match r

matchL1 :: L1 -> Parser String
matchL1 (C_L2 l2') = matchL2 l2'
matchL1 (Concat l2' l1') = do
    s1 <- matchL2 l2'
    s2 <- matchL1 l1'
    return $ s1 ++ s2

matchL2 :: L2 -> Parser String
matchL2 (C_L3 l3') = matchL3 l3'
matchL2 (Star l3') = do
    s <- matchL3 l3'
    ss <- matchL2 $ Star l3'
    return $ s ++ ss
    <|> return ""

matchL3 :: L3 -> Parser String
matchL3 (C_L4 l4') = matchL4 l4'
matchL3 (Paren r) = match r

matchL4 :: L4 -> Parser String
matchL4 A = "a" <$ char 'a'
matchL4 B = "b" <$ char 'b'

data Expr = Val Int | Add Expr Expr | Mul Expr Expr | Equal Expr Expr deriving Show
data Bims = Skip | Assign String Expr | Seq Bims Bims | If Expr Bims Bims | While Expr Bims deriving Show

expr :: Parser Expr
expr = do
    t <- nat
    do
        char '+'
        Add (Val t) <$> expr
        <|> do
            char '*'
            Mul (Val t) <$> expr
            <|> do
                char '='
                Equal (Val t) <$> expr
                <|> return (Val t)

oneOf :: [Parser a] -> Parser a
oneOf = foldr (<|>) empty

bims :: Parser Bims
bims = oneOf [skip, assign, seq', if', while]
  where
    skip = Skip <$ string "skip"
    assign = do
        v <- ident
        string ":="
        Assign v <$> expr
    seq' = do
        b1 <- bims
        char ';'
        Seq b1 <$> bims
    if' = do
        string "if ("
        e <- expr
        string ") {"
        b1 <- bims
        string "} else {"
        b2 <- bims
        char '}'
        return $ If e b1 b2
    while = do
        string "while ("
        e <- expr
        string ") {"
        b <- bims
        char '}'
        return $ While e b
